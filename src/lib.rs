//! Vanua Programming Language
//!
//! A strongly typed, object-oriented language with Lambda Calculus foundations
//! that runs on its own custom virtual machine.

pub mod ast;
pub mod codegen;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod stdlib;
pub mod typechecker;
pub mod vm;

use std::fs;
use std::path::Path;

use error::{VanuaError, VanuaResult};

/// Compilation phases that can be monitored or controlled
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilationPhase {
    /// Lexical analysis (tokenization)
    Lexing,
    /// Parsing (AST construction)
    Parsing,
    /// Type checking
    TypeChecking,
    /// Code generation (bytecode)
    CodeGeneration,
    /// Execution in VM
    Execution,
}

/// The main compiler pipeline that processes Vanua source code
pub struct Compiler {
    /// Source code to compile
    pub source: String,
    /// Optional file path (used for error reporting)
    pub file_path: Option<String>,
    /// Enable verbose debug output
    pub debug_mode: bool,
    /// Current compilation phase
    pub current_phase: CompilationPhase,
}

impl Compiler {
    /// Create a new compiler instance from source code string
    pub fn new(source: String, file_path: Option<String>) -> Self {
        Self {
            source,
            file_path,
            debug_mode: false,
            current_phase: CompilationPhase::Lexing,
        }
    }

    /// Create a new compiler instance with debug mode enabled
    pub fn new_with_debug(source: String, file_path: Option<String>) -> Self {
        Self {
            source,
            file_path,
            debug_mode: true,
            current_phase: CompilationPhase::Lexing,
        }
    }

    /// Create a new compiler instance from a file
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, VanuaError> {
        let path_str = path.as_ref().to_string_lossy().to_string();
        let extension = path_str.split(".").last().unwrap_or("");
        if extension != "va" {
            return Err(VanuaError::IoError {
                message: format!("Invalid file extension: {}. Expected '.va'", extension),
            });
        }
        let source = fs::read_to_string(&path).map_err(|e| VanuaError::IoError {
            message: format!("Failed to read file {}: {}", path_str, e),
        })?;

        Ok(Self::new(source, Some(path_str)))
    }

    pub fn from_file_with_debug<P: AsRef<Path>>(path: P) -> Result<Self, VanuaError> {
        let compiler = Self::from_file(path)?;
        Ok(Self {
            debug_mode: true,
            ..compiler
        })
    }

    pub fn compile(&mut self) -> VanuaResult<vm::ByteCode> {
        self.current_phase = CompilationPhase::Lexing;
        if self.debug_mode {
            println!("[Compiler] Starting lexical analysis");
        }
        let tokens = lexer::tokenize(&self.source)?;

        self.current_phase = CompilationPhase::Parsing;
        if self.debug_mode {
            println!("[Compiler] Starting parsing");
        }
        let ast = parser::parse(tokens)?;

        self.current_phase = CompilationPhase::TypeChecking;
        if self.debug_mode {
            println!("[Compiler] Starting type checking");
        }
        let typed_ast = typechecker::typecheck(ast)?;

        self.current_phase = CompilationPhase::CodeGeneration;
        if self.debug_mode {
            println!("[Compiler] Starting code generation");
        }
        let bytecode = codegen::generate(typed_ast)?;

        if self.debug_mode {
            println!("[Compiler] Compilation completed successfully");
        }

        Ok(bytecode)
    }

    pub fn compile_and_run(&mut self) -> VanuaResult<vm::Value> {
        self.compile_and_run_with_args(Vec::new())
    }

    pub fn compile_and_run_with_args(&mut self, args: Vec<String>) -> VanuaResult<vm::Value> {
        let bytecode = self.compile()?;
        let mut vm = vm::VirtualMachine::new();

        vm.set_debug_mode(self.debug_mode);

        if let Err(e) = vm.init_async_runtime() {
            if self.debug_mode {
                println!(
                    "[Compiler] Warning: Failed to initialize async runtime: {}",
                    e
                );
            }
        }

        vm.set_globals(stdlib::init_stdlib());
        vm.set_command_line_args(args);

        self.current_phase = CompilationPhase::Execution;
        if self.debug_mode {
            println!("[Compiler] Starting VM execution");
        }

        let result = vm.execute(bytecode);

        if self.debug_mode && result.is_ok() {
            println!("[Compiler] Execution completed successfully");
        }

        if result.is_ok() {
            let active_count = vm.get_active_async_task_count();
            if active_count > 0 {
                if self.debug_mode {
                    println!(
                        "[Compiler] Waiting for {} active async tasks to complete...",
                        active_count
                    );
                }
                if let Err(wait_error) = vm.wait_for_async_tasks(2000) {
                    // TODO refactor this
                    if self.debug_mode {
                        println!("[Compiler] Warning: {}", wait_error);
                    }
                } else if self.debug_mode {
                    println!("[Compiler] All async tasks completed successfully.");
                }
            }
        }

        vm.shutdown_async_runtime();

        result
    }

    /// Gets the current phase of compilation
    pub fn get_phase(&self) -> CompilationPhase {
        self.current_phase
    }

    /// Determines if an error is relevant to the current compilation phase
    pub fn is_phase_error(&self, error: &VanuaError) -> bool {
        match self.current_phase {
            CompilationPhase::Lexing => matches!(error, VanuaError::LexError { .. }),
            CompilationPhase::Parsing => matches!(error, VanuaError::ParseError { .. }),
            CompilationPhase::TypeChecking => matches!(error, VanuaError::TypeError { .. }),
            CompilationPhase::CodeGeneration => matches!(error, VanuaError::CodegenError { .. }),
            CompilationPhase::Execution => error.is_runtime(),
        }
    }
}

/// Version information for the Vanua compiler
pub fn version() -> &'static str {
    "0.1.0"
}

/// Shorthand function to compile source code to bytecode
pub fn compile(source: &str) -> VanuaResult<vm::ByteCode> {
    let tokens = lexer::tokenize(source)?;
    let program = parser::parse(tokens)?;
    let typed_program = typechecker::typecheck(program)?;
    let bytecode = codegen::generate(typed_program)?;

    Ok(bytecode)
}

/// Shorthand function to compile and execute source code
pub fn run(source: &str) -> VanuaResult<vm::Value> {
    let bytecode = compile(source)?;
    let mut vm = vm::VirtualMachine::new();

    let _ = vm.init_async_runtime();

    vm.set_globals(stdlib::init_stdlib());
    let result = vm.execute(bytecode);

    if result.is_ok() {
        let active_count = vm.get_active_async_task_count();
        if active_count > 0 {
            println!(
                "[Compiler] Waiting for {} active async tasks to complete...",
                active_count
            );
            if let Err(wait_error) = vm.wait_for_async_tasks(5000) {
                // TODO refactor this
                println!("[Compiler] Warning: {}", wait_error);
            } else {
                println!("[Compiler] All async tasks completed successfully.");
            }
        }
    }

    result
}
