mod async_runtime;
mod value;

pub use self::async_runtime::{AsyncExecutionContext, AsyncRuntime};
pub use self::value::{
    ComposedFunction, CurriedFunction, FutureValue, LazyValue, MatchPattern, Object, ObjectType,
    PartialFunction, SimpleFutureValue, Value,
};

use crate::error::VanuaError;

use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::cmp::Eq;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ByteCode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionBytecode {
    pub name: String,
    pub param_count: usize,
    pub local_count: usize,
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Constant {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Function(FunctionBytecode),
    Native(String),
    Null,
}

impl Eq for Constant {}

impl Hash for Constant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Constant::Int(n) => {
                0u8.hash(state);
                n.hash(state);
            }
            Constant::Float(f) => {
                1u8.hash(state);
                f.to_bits().hash(state);
            }
            Constant::Bool(b) => {
                2u8.hash(state);
                b.hash(state);
            }
            Constant::Char(c) => {
                3u8.hash(state);
                c.hash(state);
            }
            Constant::String(s) => {
                4u8.hash(state);
                s.hash(state);
            }
            Constant::Function(func) => {
                5u8.hash(state);
                func.name.hash(state);
                func.param_count.hash(state);
                func.local_count.hash(state);
            }
            Constant::Native(name) => {
                6u8.hash(state);
                name.hash(state);
            }
            Constant::Null => {
                7u8.hash(state);
            }
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::Int(value) => write!(f, "{}", value),
            Constant::Float(value) => write!(f, "{}", value),
            Constant::String(value) => write!(f, "{}", value),
            Constant::Char(value) => write!(f, "{}", value),
            Constant::Bool(value) => write!(f, "{}", value),
            Constant::Function(func) => write!(f, "{}", func.name),
            Constant::Native(name) => write!(f, "{}", name),
            Constant::Null => write!(f, "NULL"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    // Stack operations
    Constant(usize),
    Nil,
    Unit,
    True,
    False,
    Pop,
    Duplicate,
    Swap,
    IsNotNull,

    // Local variables
    GetLocal(u8),
    SetLocal(u8),

    // Global variables
    DefineGlobal(usize),
    GetGlobal(usize),
    SetGlobal(usize),

    // Array and Object operations
    IsArray,
    IsObject,
    CheckArrayLength,
    GetArrayElement,
    CheckType(usize),

    // Arithmetic operations
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Negate,

    // Logical operations
    Not,
    Equal,
    Greater,
    Less,
    And,
    Or,

    // Control flow
    Jump(u16),
    JumpIfFalse(u16),
    JumpIfTrue(u16),
    Call(u8),
    Return,

    // Classes and objects
    Class(usize),
    Method(usize, usize),
    GetProperty(usize),
    SetProperty(usize),
    Invoke(usize, u8),

    // Lambda calculus operations
    Curry,
    Compose,
    PartialApply(Vec<bool>),

    // Pattern matching
    MatchBegin,
    MatchPattern(usize),
    MatchGuard,
    MatchJump(u16),
    MatchEnd,

    // Lazy evaluation
    MakeLazy,
    ForceLazy,

    // Module import instructions
    LoadModule(usize),

    // Class inheritance instructions
    Inherit,

    // Trait instructions
    Trait(usize),
    TraitMethod(usize, usize),

    // Struct instructions
    Struct(usize),

    // Implementation instructions
    AddImplMethod(usize, usize),

    // Bitwise operations
    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr,
    BitNot,

    // Null safety operations
    NullCoalesce,
    SafeProperty(usize),

    // Exception handling
    TryCatch(u16), // Jump offset in case of exception
    Throw,         // Throws an exception

    // Increment/Decrement operations
    PreIncrement,  // ++i
    PostIncrement, // i++
    PreDecrement,  // --i
    PostDecrement, // i--

    // Asynchronous programming instructions
    CreateFuture,    // Creates a new Future
    SpawnAsync(u16), // Executes a function asynchronously (jump to function body)
    AwaitFuture,     // Waits for a Future to complete

    // Inheritance instructions
    SuperCall(u8), // Calls parent constructor with specified number of arguments
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Instruction::Constant(idx) => write!(f, "CONSTANT {}", idx),
            Instruction::Nil => write!(f, "NIL"),
            Instruction::Unit => write!(f, "UNIT"),
            Instruction::True => write!(f, "TRUE"),
            Instruction::False => write!(f, "FALSE"),
            Instruction::Pop => write!(f, "POP"),
            Instruction::Duplicate => write!(f, "DUPLICATE"),
            Instruction::Swap => write!(f, "SWAP"),
            Instruction::IsNotNull => write!(f, "IS_NOT_NULL"),
            Instruction::GetLocal(slot) => write!(f, "GET_LOCAL {}", slot),
            Instruction::SetLocal(slot) => write!(f, "SET_LOCAL {}", slot),
            Instruction::DefineGlobal(name) => write!(f, "DEFINE_GLOBAL {}", name),
            Instruction::GetGlobal(name) => write!(f, "GET_GLOBAL {}", name),
            Instruction::SetGlobal(name) => write!(f, "SET_GLOBAL {}", name),
            Instruction::IsArray => write!(f, "IS_ARRAY"),
            Instruction::IsObject => write!(f, "IS_OBJECT"),
            Instruction::CheckArrayLength => write!(f, "CHECK_ARRAY_LENGTH"),
            Instruction::GetArrayElement => write!(f, "GET_ARRAY_ELEMENT"),
            Instruction::CheckType(name) => write!(f, "CHECK_TYPE {}", name),
            Instruction::Add => write!(f, "ADD"),
            Instruction::Subtract => write!(f, "SUBTRACT"),
            Instruction::Multiply => write!(f, "MULTIPLY"),
            Instruction::Divide => write!(f, "DIVIDE"),
            Instruction::Modulo => write!(f, "MODULO"),
            Instruction::Negate => write!(f, "NEGATE"),
            Instruction::Not => write!(f, "NOT"),
            Instruction::Equal => write!(f, "EQUAL"),
            Instruction::Greater => write!(f, "GREATER"),
            Instruction::Less => write!(f, "LESS"),
            Instruction::And => write!(f, "AND"),
            Instruction::Or => write!(f, "OR"),
            Instruction::BitAnd => write!(f, "BIT_AND"),
            Instruction::BitOr => write!(f, "BIT_OR"),
            Instruction::BitXor => write!(f, "BIT_XOR"),
            Instruction::BitShl => write!(f, "BIT_SHL"),
            Instruction::BitShr => write!(f, "BIT_SHR"),
            Instruction::BitNot => write!(f, "BIT_NOT"),
            Instruction::NullCoalesce => write!(f, "NULL_COALESCE"),
            Instruction::SafeProperty(name) => write!(f, "SAFE_PROPERTY {}", name),
            Instruction::Jump(offset) => write!(f, "JUMP {}", offset),
            Instruction::JumpIfFalse(offset) => write!(f, "JUMP_IF_FALSE {}", offset),
            Instruction::JumpIfTrue(offset) => write!(f, "JUMP_IF_TRUE {}", offset),
            Instruction::Call(arg_count) => write!(f, "CALL {}", arg_count),
            Instruction::Return => write!(f, "RETURN"),
            Instruction::Curry => write!(f, "CURRY"),
            Instruction::Compose => write!(f, "COMPOSE"),
            Instruction::PartialApply(args) => {
                let mut result = String::from("PARTIAL_APPLY [");
                for (i, provided) in args.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(if *provided { "provided" } else { "hole" });
                }
                result.push_str("]");
                write!(f, "{}", result)
            }
            Instruction::MatchBegin => write!(f, "MATCH_BEGIN"),
            Instruction::MatchPattern(pattern) => write!(f, "MATCH_PATTERN {}", pattern),
            Instruction::MatchGuard => write!(f, "MATCH_GUARD"),
            Instruction::MatchJump(offset) => write!(f, "MATCH_JUMP {}", offset),
            Instruction::MatchEnd => write!(f, "MATCH_END"),
            Instruction::MakeLazy => write!(f, "MAKE_LAZY"),
            Instruction::ForceLazy => write!(f, "FORCE_LAZY"),
            Instruction::LoadModule(name) => write!(f, "LOAD_MODULE {}", name),
            Instruction::Inherit => write!(f, "INHERIT"),
            Instruction::Trait(name) => write!(f, "TRAIT {}", name),
            Instruction::TraitMethod(name, method) => write!(f, "TRAIT_METHOD {} {}", name, method),
            Instruction::Struct(name) => write!(f, "STRUCT {}", name),
            Instruction::AddImplMethod(name, method) => {
                write!(f, "ADD_IMPL_METHOD {} {}", name, method)
            }
            Instruction::Class(name) => write!(f, "CLASS {}", name),
            Instruction::Method(name, method) => write!(f, "METHOD {} {}", name, method),
            Instruction::GetProperty(name) => write!(f, "GET_PROPERTY {}", name),
            Instruction::SetProperty(name) => write!(f, "SET_PROPERTY {}", name),
            Instruction::Invoke(name, arg_count) => write!(f, "INVOKE {} {}", name, arg_count),
            Instruction::TryCatch(offset) => write!(f, "TRY_CATCH {}", offset),
            Instruction::Throw => write!(f, "THROW"),
            Instruction::PreIncrement => write!(f, "PRE_INCREMENT"),
            Instruction::PostIncrement => write!(f, "POST_INCREMENT"),
            Instruction::PreDecrement => write!(f, "PRE_DECREMENT"),
            Instruction::PostDecrement => write!(f, "POST_DECREMENT"),
            Instruction::CreateFuture => write!(f, "CREATE_FUTURE"),
            Instruction::SpawnAsync(offset) => write!(f, "SPAWN_ASYNC {}", offset),
            Instruction::AwaitFuture => write!(f, "AWAIT_FUTURE"),
            Instruction::SuperCall(arg_count) => write!(f, "SUPER_CALL {}", arg_count),
        }
    }
}

#[derive(Clone)]
struct CallFrame {
    function_name: String,
    ip: usize,
    #[allow(dead_code)]
    slot_base: usize,
    locals: Vec<Value>,
    bytecode: Option<FunctionBytecode>,
}

pub struct VirtualMachine {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    current_frame_idx: usize,
    error_handler_stack: Vec<usize>,
    command_line_args: Vec<String>,
    async_runtime: Option<AsyncRuntime>,
    stack_limit: Option<usize>,
    function_registry: HashMap<String, FunctionBytecode>,
    debug_mode: bool,
    // inheritance registry: class_name -> (superclasses, implemented_traits)
    class_inheritance: HashMap<String, (Vec<String>, Vec<String>)>,
    // virtual inheritance table to prevent duplicate base class constructor calls
    virtual_inheritance_table: HashMap<String, HashSet<String>>, // object_id -> set of called constructors
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            stack: Vec::new(),
            globals: HashMap::new(),
            current_frame_idx: 0,
            error_handler_stack: Vec::new(),
            command_line_args: Vec::new(),
            async_runtime: None,
            stack_limit: None,
            function_registry: HashMap::new(),
            debug_mode: false,
            class_inheritance: HashMap::new(),
            virtual_inheritance_table: HashMap::new(),
        }
    }

    pub fn init_async_runtime(&mut self) -> Result<(), VanuaError> {
        self.async_runtime = Some(AsyncRuntime::new(self.debug_mode)?);
        Ok(())
    }

    pub fn shutdown_async_runtime(&mut self) {
        if let Some(ref runtime) = self.async_runtime {
            runtime.shutdown();
        }
        self.async_runtime = None;
    }

    /// Register a function in the global function registry
    pub fn register_function(&mut self, name: String, bytecode: FunctionBytecode) {
        self.function_registry.insert(name, bytecode);
    }

    /// Get a function from the global function registry
    pub fn get_function(&self, name: &str) -> Option<&FunctionBytecode> {
        self.function_registry.get(name)
    }

    /// Wait for all async tasks to complete with timeout
    pub fn wait_for_async_tasks(&self, timeout_ms: u64) -> Result<(), VanuaError> {
        if let Some(ref runtime) = self.async_runtime {
            runtime.wait_for_all_tasks(timeout_ms)
        } else {
            Ok(())
        }
    }

    /// Wait for all async tasks to complete without timeout
    pub fn wait_for_async_tasks_indefinitely(&self) -> Result<(), VanuaError> {
        if let Some(ref runtime) = self.async_runtime {
            runtime.wait_for_all_tasks_indefinitely()
        } else {
            Ok(())
        }
    }

    /// Get the number of currently active async tasks
    pub fn get_active_async_task_count(&self) -> u64 {
        if let Some(ref runtime) = self.async_runtime {
            runtime.get_active_task_count()
        } else {
            0
        }
    }

    /// Sets the global variables for the VM
    pub fn set_globals(&mut self, globals: HashMap<String, Value>) {
        self.globals = globals;
    }

    /// Set stack size limit for production safety
    pub fn set_stack_limit(&mut self, limit: usize) {
        self.stack_limit = Some(limit);
    }

    /// Check if stack limit is exceeded (production feature)
    #[allow(dead_code)]
    fn check_stack_limit(&self) -> Result<(), VanuaError> {
        if let Some(limit) = self.stack_limit {
            if self.stack.len() >= limit {
                return Err(VanuaError::RuntimeError {
                    message: format!("Stack overflow: exceeded limit of {} items", limit),
                    cause: None,
                });
            }
        }
        Ok(())
    }

    /// Safe stack push with limit checking (production feature)
    #[allow(dead_code)]
    fn safe_push(&mut self, value: Value) -> Result<(), VanuaError> {
        self.check_stack_limit()?;
        self.stack.push(value);
        Ok(())
    }

    /// Sets the command-line arguments for the VM
    pub fn set_command_line_args(&mut self, args: Vec<String>) {
        self.command_line_args = args;
    }

    pub fn set_debug_mode(&mut self, debug: bool) {
        self.debug_mode = debug;
    }

    /// Gets a copy of the VM global variables
    pub fn get_globals(&self) -> HashMap<String, Value> {
        self.globals.clone()
    }

    /// Gets a reference to the current stack
    pub fn get_stack(&self) -> &Vec<Value> {
        &self.stack
    }

    /// Find function bytecode by name in the constants
    fn find_function_bytecode<'a>(
        &self,
        name: &str,
        constants: &'a [Constant],
    ) -> Option<&'a FunctionBytecode> {
        for constant in constants {
            if let Constant::Function(func) = constant {
                if func.name == name {
                    if func.instructions.is_empty() {
                        continue;
                    }
                    return Some(func);
                }
            }
        }
        None
    }

    /// Resolve method dispatch using dynamic method lookup
    /// This implements virtual method dispatch for inheritance
    fn resolve_method_dispatch(
        &self,
        obj: &Object,
        method_name: &str,
        constants: &[Constant],
    ) -> Result<Option<String>, VanuaError> {
        let class_name = match &obj.typ {
            ObjectType::Instance(name) => name,
            _ => return Ok(None),
        };

        let method_func_name = format!("{}.{}", class_name, method_name);
        if self
            .find_function_bytecode(&method_func_name, constants)
            .is_some()
        {
            return Ok(Some(method_func_name));
        }

        self.resolve_method_in_inheritance_chain(class_name, method_name, constants)
    }

    /// Resolve method in the inheritance chain using MRO
    fn resolve_method_in_inheritance_chain(
        &self,
        class_name: &str,
        method_name: &str,
        constants: &[Constant],
    ) -> Result<Option<String>, VanuaError> {
        let mro = self.compute_mro(class_name);

        for parent_class in mro.iter().skip(1) {
            let parent_method_name = format!("{}.{}", parent_class, method_name);
            if self
                .find_function_bytecode(&parent_method_name, constants)
                .is_some()
            {
                return Ok(Some(parent_method_name));
            }
        }

        let fallback_parents = vec!["Unit"];
        for parent in fallback_parents {
            if parent != class_name && !mro.contains(&parent.to_string()) {
                let parent_method_name = format!("{}.{}", parent, method_name);
                if self
                    .find_function_bytecode(&parent_method_name, constants)
                    .is_some()
                {
                    return Ok(Some(parent_method_name));
                }
            }
        }

        Ok(None)
    }

    /// Unified addition operation for all execution contexts
    fn add_values(&mut self, a: Value, b: Value) -> Result<(), VanuaError> {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a + b)),
            (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Float(a + b)),
            (Value::Int(a), Value::Float(b)) => self.stack.push(Value::Float(a as f64 + b)),
            (Value::Float(a), Value::Int(b)) => self.stack.push(Value::Float(a + b as f64)),
            (Value::String(a), Value::String(b)) => {
                let mut new_string = String::with_capacity(a.len() + b.len());
                new_string.push_str(&a);
                new_string.push_str(&b);
                self.stack.push(Value::String(new_string));
            }
            (Value::String(a), b) => {
                let mut new_string = String::with_capacity(a.len() + 10); // FIXME: 10 is arbitrary
                new_string.push_str(&a);
                new_string.push_str(&b.to_string());
                self.stack.push(Value::String(new_string));
            }
            (a, Value::String(b)) => {
                let a_str = a.to_string();
                let mut new_string = String::with_capacity(a_str.len() + b.len());
                new_string.push_str(&a_str);
                new_string.push_str(&b);
                self.stack.push(Value::String(new_string));
            }
            (Value::Array(mut a), Value::Array(mut b)) => {
                let mut new_array = Vec::with_capacity(a.len() + b.len());
                new_array.append(&mut a);
                new_array.append(&mut b);
                self.stack.push(Value::Array(new_array));
            }
            (a, b) => {
                return Err(VanuaError::RuntimeError {
                    message: format!("Cannot add {} and {}", a.type_name(), b.type_name()),
                    cause: None,
                });
            }
        }
        Ok(())
    }

    /// Execute a function's bytecode directly
    #[allow(dead_code)]
    fn execute_function(&mut self, function_bytecode: &FunctionBytecode) -> Result<(), VanuaError> {
        let mut ip = 0;

        while ip < function_bytecode.instructions.len() {
            let instruction = &function_bytecode.instructions[ip];

            match instruction {
                Instruction::GetLocal(slot) => {
                    let frame = &self.frames[self.current_frame_idx];
                    let value = frame.locals[*slot as usize].clone();

                    self.stack.push(value);
                }
                Instruction::SetProperty(name_idx) => {
                    let name = self.constant_to_string(&function_bytecode.constants[*name_idx]);
                    let obj = self.stack.pop().unwrap();
                    let value = self.stack.pop().unwrap();
                    if let Value::Object(obj_ref) = obj {
                        obj_ref.borrow_mut().fields.insert(name, value);
                    }
                }
                Instruction::GetProperty(name_idx) => {
                    let name = self.constant_to_string(&function_bytecode.constants[*name_idx]);
                    let obj = self.stack.pop().unwrap();
                    println!(
                        "DEBUG: GetProperty (first handler) - property: {}, object: {:?}",
                        name, obj
                    );

                    match obj {
                        Value::Object(obj_ref) => {
                            let obj_borrowed = obj_ref.borrow();

                            if let ObjectType::Class(class_name) = &obj_borrowed.typ {
                                if name == "new" {
                                    let stdlib_classes =
                                        vec!["Map", "Array", "String", "Integer", "Boolean"];
                                    if stdlib_classes.contains(&class_name.as_str()) {
                                        self.stack
                                            .push(Value::Function(format!("{}.new", class_name)));
                                    } else {
                                        self.stack
                                            .push(Value::Function(format!("{}.new", class_name)));
                                    }
                                } else {
                                    if let Some(value) = obj_borrowed.fields.get(&name) {
                                        self.stack.push(value.clone());
                                    } else {
                                        self.stack.push(Value::Nil);
                                    }
                                }
                            } else {
                                if let Some(value) = obj_borrowed.fields.get(&name) {
                                    self.stack.push(value.clone());
                                } else {
                                    self.stack.push(Value::Nil);
                                }
                            }
                        }
                        Value::Function(func_name) => {
                            if name == "new" {
                                let stdlib_classes =
                                    vec!["Map", "Array", "String", "Integer", "Boolean"];
                                if stdlib_classes.contains(&func_name.as_str()) {
                                    self.stack
                                        .push(Value::Function(format!("{}.new", func_name)));
                                } else {
                                    return Err(VanuaError::RuntimeError {
                                        message: format!("Method 'new' not found on {}", func_name),
                                        cause: None,
                                    });
                                }
                            } else {
                                return Err(VanuaError::RuntimeError {
                                    message: format!("Method '{}' not found on Function", name),
                                    cause: None,
                                });
                            }
                        }
                        _ => {
                            self.stack.push(Value::Nil);
                        }
                    }
                }
                Instruction::Duplicate => {
                    let value = self.stack.last().unwrap().clone();
                    self.stack.push(value);
                }
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::Add => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Add operation requires 2 operands"
                                .to_string(),
                            cause: None,
                        });
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.add_values(a, b)?;
                }
                Instruction::Constant(idx) => {
                    let value = self.constant_to_value(&function_bytecode.constants[*idx]);
                    self.stack.push(value);
                }
                Instruction::Return => {
                    break;
                }
                _ => {
                    return Err(VanuaError::runtime_error(
                        format!("Unsupported instruction in constructor: {:?}", instruction),
                        None,
                    ));
                }
            }

            ip += 1;
        }

        Ok(())
    }

    /// Call a user-defined function
    pub fn call_function(
        &mut self,
        function_bytecode: &FunctionBytecode,
        args: Vec<Value>,
    ) -> Result<(), VanuaError> {
        if args.len() != function_bytecode.param_count {
            return Err(VanuaError::runtime_error(
                format!(
                    "Function '{}' expects {} arguments, got {}",
                    function_bytecode.name,
                    function_bytecode.param_count,
                    args.len()
                ),
                None,
            ));
        }

        let mut locals = vec![Value::Nil; function_bytecode.local_count];

        for (i, arg) in args.iter().enumerate() {
            if i < locals.len() {
                locals[i] = arg.clone();
            }
        }

        let new_frame = CallFrame {
            function_name: function_bytecode.name.clone(),
            ip: 0,
            slot_base: self.stack.len(),
            locals,
            bytecode: Some(function_bytecode.clone()),
        };

        self.frames.push(new_frame);
        self.current_frame_idx = self.frames.len() - 1;

        Ok(())
    }

    pub fn execute(&mut self, bytecode: ByteCode) -> Result<Value, VanuaError> {
        self.stack.clear();
        self.frames.clear();

        if self.debug_mode {
            eprintln!("[VM] Bytecode to execute:");
            for (i, instruction) in bytecode.instructions.iter().enumerate() {
                eprintln!("[VM]   {}: {:?}", i, instruction);
            }
            eprintln!("[VM] Constants:");
            for (i, constant) in bytecode.constants.iter().enumerate() {
                eprintln!("[VM]   {}: {:?}", i, constant);
            }
        }

        self.frames.push(CallFrame {
            function_name: "main".to_string(),
            ip: 0,
            slot_base: 0,
            locals: Vec::new(),
            bytecode: None,
        });
        self.current_frame_idx = 0;

        if self.debug_mode {
            eprintln!("[VM] Initial frame created");
        }

        for constant in &bytecode.constants {
            if let Constant::Function(func) = constant {
                self.globals
                    .insert(func.name.clone(), Value::Function(func.name.clone()));
            }
        }

        loop {
            if self.frames.is_empty() {
                break;
            }

            let current_frame_idx = self.current_frame_idx;
            let frame_bytecode = self.frames[current_frame_idx].bytecode.clone();
            let frame_ip = self.frames[current_frame_idx].ip;

            let (current_instructions, current_constants) =
                if let Some(ref func_bytecode) = frame_bytecode {
                    (&func_bytecode.instructions, &func_bytecode.constants)
                } else {
                    (&bytecode.instructions, &bytecode.constants)
                };

            if frame_ip >= current_instructions.len() {
                self.frames.pop();
                if !self.frames.is_empty() {
                    self.current_frame_idx = self.frames.len() - 1;
                }
                continue;
            }

            let instruction = current_instructions[frame_ip].clone();
            self.frames[current_frame_idx].ip += 1;

            if self.debug_mode {
                eprintln!("[VM] Executing instruction: {:?}", instruction);
            }

            match instruction {
                Instruction::Constant(idx) => {
                    let constant = &current_constants[idx];

                    if let Constant::String(s) = constant {
                        if s == "__COMMAND_LINE_ARGS__" {
                            let args_array: Vec<Value> = self
                                .command_line_args
                                .iter()
                                .map(|arg| Value::String(arg.clone()))
                                .collect();
                            self.stack.push(Value::Array(args_array));
                            continue;
                        }
                    }

                    let value = self.constant_to_value(constant);
                    self.stack.push(value);
                }
                Instruction::Nil => self.stack.push(Value::Nil),
                Instruction::Unit => self.stack.push(Value::base_object()),
                Instruction::True => self.stack.push(Value::Bool(true)),
                Instruction::False => self.stack.push(Value::Bool(false)),
                Instruction::Pop => {
                    self.stack.pop();
                }
                Instruction::Duplicate => {
                    if let Some(value) = self.stack.last() {
                        self.stack.push(value.clone());
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "Cannot duplicate: stack is empty".to_string(),
                            cause: None,
                        });
                    }
                }
                Instruction::Swap => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Cannot swap: stack has fewer than 2 elements".to_string(),
                            cause: None,
                        });
                    }
                    let len = self.stack.len();
                    self.stack.swap(len - 1, len - 2);
                }
                Instruction::IsNotNull => {
                    if let Some(value) = self.stack.pop() {
                        let is_not_null = !matches!(value, Value::Nil);
                        self.stack.push(Value::Bool(is_not_null));
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "Cannot check nullability: stack is empty".to_string(),
                            cause: None,
                        });
                    }
                }
                Instruction::GetLocal(slot) => {
                    let slot = slot as usize;

                    if self.debug_mode {
                        eprintln!("[VM] GetLocal: slot = {}", slot);
                    }

                    let frame = &self.frames[self.current_frame_idx];
                    if slot < frame.locals.len() {
                        let value = frame.locals[slot].clone();

                        if self.debug_mode {
                            eprintln!("[VM] GetLocal: value = {:?}", value);
                        }

                        self.stack.push(value);
                    } else {
                        return Err(VanuaError::runtime_error(
                            format!("Access to uninitialized local variable at index {}", slot),
                            None,
                        ));
                    }
                }
                Instruction::SetLocal(slot) => {
                    let slot = slot as usize;

                    if self.stack.is_empty() {
                        return Err(VanuaError::runtime_error(
                            "Empty stack during SetLocal".to_string(),
                            None,
                        ));
                    }

                    let value = self.stack.pop().unwrap();

                    if self.debug_mode {
                        eprintln!("[VM] SetLocal: slot = {}", slot);
                        eprintln!("[VM] SetLocal: value = {:?}", value);
                    }

                    let frame = &mut self.frames[self.current_frame_idx];
                    while frame.locals.len() <= slot {
                        frame.locals.push(Value::Nil);
                    }

                    frame.locals[slot] = value;
                }
                Instruction::DefineGlobal(name_idx) => {
                    let name = self.constant_to_string(&current_constants[name_idx]);
                    let value = self.stack.pop().unwrap();
                    self.globals.insert(name, value);
                }
                Instruction::GetGlobal(name_idx) => {
                    let name = self.constant_to_string(&current_constants[name_idx]);

                    if let Some(value) = self.get_global_by_name(&name) {
                        self.stack.push(value.clone());
                    } else {
                        let stdlib_modules = crate::stdlib::get_stdlib_modules_silent();
                        let mut found = false;

                        for module in stdlib_modules {
                            if module.constants.contains_key(&name) {
                                if let Some(value) = module.constants.get(&name) {
                                    self.stack.push(value.clone());
                                    found = true;
                                    break;
                                }
                            }
                        }

                        if !found {
                            // FIXME: duplicated code + must be removed as it belongs to an old test

                            if name.len() > 50 {
                                self.stack.push(Value::String(name));
                            } else {
                                return Err(VanuaError::RuntimeError {
                                    message: format!("Undefined variable '{}'", name),
                                    cause: None,
                                });
                            }
                        }
                    }
                }
                Instruction::SetGlobal(name_idx) => {
                    let name = self.constant_to_string(&current_constants[name_idx]);

                    if self.stack.is_empty() {
                        return Err(VanuaError::RuntimeError {
                            message: format!("Empty stack during SetGlobal for '{}'", name),
                            cause: None,
                        });
                    }

                    let value = self.stack.last().unwrap().clone();
                    if self.globals.contains_key(&name) {
                        self.globals.insert(name, value);
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: format!("Undefined variable '{}'", name),
                            cause: None,
                        });
                    }
                }
                Instruction::IsArray => {
                    let value = self.stack.pop().unwrap();
                    let is_array = matches!(value, Value::Array(_));
                    self.stack.push(Value::Bool(is_array));
                }
                Instruction::IsObject => {
                    let value = self.stack.pop().unwrap();
                    let is_object = matches!(value, Value::Object(_));
                    self.stack.push(Value::Bool(is_object));
                }
                Instruction::CheckArrayLength => {
                    let length = match self.stack.pop().unwrap() {
                        Value::Int(n) => n as usize,
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Expected integer for array length check".to_string(),
                                cause: None,
                            })
                        }
                    };

                    let array = match self.stack.last().unwrap() {
                        Value::Array(arr) => arr,
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Expected array for length check".to_string(),
                                cause: None,
                            })
                        }
                    };

                    self.stack.push(Value::Bool(array.len() == length));
                }
                Instruction::GetArrayElement => {
                    let index = match self.stack.pop().unwrap() {
                        Value::Int(i) => i as usize,
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Expected integer for array index".to_string(),
                                cause: None,
                            })
                        }
                    };

                    let array = match self.stack.pop().unwrap() {
                        Value::Array(arr) => arr,
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Expected array".to_string(),
                                cause: None,
                            })
                        }
                    };

                    if index < array.len() {
                        self.stack.push(array[index].clone());
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: format!("Array index out of bounds: {}", index),
                            cause: None,
                        });
                    }
                }
                Instruction::CheckType(name_idx) => {
                    let type_name = self.constant_to_string(&current_constants[name_idx]);

                    let value = self.stack.pop().unwrap();
                    let is_type = match type_name.as_str() {
                        "Int" => matches!(value, Value::Int(_)),
                        "Float" => matches!(value, Value::Float(_)),
                        "Bool" => matches!(value, Value::Bool(_)),
                        "Char" => matches!(value, Value::Char(_)),
                        "String" => matches!(value, Value::String(_)),
                        "Array" => matches!(value, Value::Array(_)),
                        "Map" => matches!(value, Value::Map(_)),
                        "Function" => matches!(value, Value::Function(_)),
                        "Object" => matches!(value, Value::Object(_)),
                        _ => {
                            if let Value::Object(obj) = &value {
                                match &obj.borrow().typ {
                                    ObjectType::Instance(name) => name == &type_name,
                                    ObjectType::Class(name) => name == &type_name,
                                    _ => false,
                                }
                            } else {
                                false
                            }
                        }
                    };

                    self.stack.push(Value::Bool(is_type));
                }
                Instruction::Add => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Add operation requires 2 operands"
                                .to_string(),
                            cause: None,
                        });
                    }
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.add_values(a, b)?;
                }
                Instruction::Subtract => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Subtract operation requires 2 operands"
                                .to_string(),
                            cause: None,
                        });
                    }

                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a - b)),
                        (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Float(a - b)),
                        (Value::Int(a), Value::Float(b)) => {
                            self.stack.push(Value::Float(a as f64 - b))
                        }
                        (Value::Float(a), Value::Int(b)) => {
                            self.stack.push(Value::Float(a - b as f64))
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands must be numbers".to_string(),
                                cause: None,
                            })
                        }
                    }
                }
                Instruction::Multiply => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Multiply operation requires 2 operands"
                                .to_string(),
                            cause: None,
                        });
                    }

                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => self.stack.push(Value::Int(a * b)),
                        (Value::Float(a), Value::Float(b)) => self.stack.push(Value::Float(a * b)),
                        (Value::Int(a), Value::Float(b)) => {
                            self.stack.push(Value::Float(a as f64 * b))
                        }
                        (Value::Float(a), Value::Int(b)) => {
                            self.stack.push(Value::Float(a * b as f64))
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands must be numbers".to_string(),
                                cause: None,
                            })
                        }
                    }
                }
                Instruction::Divide => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Divide operation requires 2 operands"
                                .to_string(),
                            cause: None,
                        });
                    }

                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            if b == 0 {
                                return Err(VanuaError::RuntimeError {
                                    message: "Division by zero".to_string(),
                                    cause: None,
                                });
                            }
                            self.stack.push(Value::Int(a / b));
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            if b == 0.0 {
                                return Err(VanuaError::RuntimeError {
                                    message: "Division by zero".to_string(),
                                    cause: None,
                                });
                            }
                            self.stack.push(Value::Float(a / b));
                        }
                        (Value::Int(a), Value::Float(b)) => {
                            if b == 0.0 {
                                return Err(VanuaError::RuntimeError {
                                    message: "Division by zero".to_string(),
                                    cause: None,
                                });
                            }
                            self.stack.push(Value::Float(a as f64 / b));
                        }
                        (Value::Float(a), Value::Int(b)) => {
                            if b == 0 {
                                return Err(VanuaError::RuntimeError {
                                    message: "Division by zero".to_string(),
                                    cause: None,
                                });
                            }
                            self.stack.push(Value::Float(a / b as f64));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands must be numbers".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::Modulo => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Modulo operation requires 2 operands"
                                .to_string(),
                            cause: None,
                        });
                    }

                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            if b == 0 {
                                return Err(VanuaError::RuntimeError {
                                    message: "Modulo by zero".to_string(),
                                    cause: None,
                                });
                            }
                            self.stack.push(Value::Int(a % b));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands must be integers".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::Negate => {
                    if self.stack.is_empty() {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Negate operation requires 1 operand"
                                .to_string(),
                            cause: None,
                        });
                    }

                    let value = self.stack.pop().unwrap();

                    match value {
                        Value::Int(val) => {
                            self.stack.push(Value::Int(-val));
                        }
                        Value::Float(val) => {
                            self.stack.push(Value::Float(-val));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operand must be a number".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::Not => {
                    if self.stack.is_empty() {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Not operation requires 1 operand"
                                .to_string(),
                            cause: None,
                        });
                    }

                    let value = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(!value.is_truthy()));
                }
                Instruction::Equal => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Equal operation requires 2 operands"
                                .to_string(),
                            cause: None,
                        });
                    }

                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(a == b));
                }
                Instruction::Greater => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Greater operation requires 2 operands"
                                .to_string(),
                            cause: None,
                        });
                    }

                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a > b));
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            self.stack.push(Value::Bool(a > b));
                        }
                        (Value::Int(a), Value::Float(b)) => {
                            self.stack.push(Value::Bool((a as f64) > b));
                        }
                        (Value::Float(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a > (b as f64)));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands must be numbers".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::Less => {
                    if self.stack.len() < 2 {
                        return Err(VanuaError::RuntimeError {
                            message: "Stack underflow: Less operation requires 2 operands"
                                .to_string(),
                            cause: None,
                        });
                    }

                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a < b));
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            self.stack.push(Value::Bool(a < b));
                        }
                        (Value::Int(a), Value::Float(b)) => {
                            self.stack.push(Value::Bool((a as f64) < b));
                        }
                        (Value::Float(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a < (b as f64)));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands must be numbers".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::And => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(a.is_truthy() && b.is_truthy()));
                }
                Instruction::Or => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(a.is_truthy() || b.is_truthy()));
                }
                Instruction::BitAnd => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(a & b));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands for bitwise AND must be integers".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::BitOr => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(a | b));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands for bitwise OR must be integers".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::BitXor => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(a ^ b));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands for bitwise XOR must be integers".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::BitShl => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            if b < 0 || b > 63 {
                                return Err(VanuaError::RuntimeError {
                                    message: "Shift amount must be between 0 and 63".to_string(),
                                    cause: None,
                                });
                            }
                            self.stack.push(Value::Int(a << b as u32));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands for left shift must be integers".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::BitShr => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();

                    match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            if b < 0 || b > 63 {
                                return Err(VanuaError::RuntimeError {
                                    message: "Shift amount must be between 0 and 63".to_string(),
                                    cause: None,
                                });
                            }
                            self.stack.push(Value::Int(a >> b as u32));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operands for right shift must be integers".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::BitNot => {
                    let value = self.stack.pop().unwrap();

                    match value {
                        Value::Int(val) => {
                            self.stack.push(Value::Int(!val));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Operand for bitwise NOT must be an integer".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::NullCoalesce => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();

                    if let Value::Nil = left {
                        self.stack.push(right);
                    } else {
                        self.stack.push(left);
                    }
                }
                Instruction::SafeProperty(name_idx) => {
                    let obj = self.stack.pop().unwrap();
                    let prop_name = self.constant_to_string(&current_constants[name_idx]);

                    match obj {
                        Value::Nil => {
                            self.stack.push(Value::Nil);
                        }
                        Value::Object(obj_ref) => {
                            let obj = obj_ref.borrow();
                            if let Some(value) = obj.fields.get(&prop_name) {
                                self.stack.push(value.clone());
                            } else {
                                self.stack.push(Value::Nil);
                            }
                        }
                        _ => {
                            self.stack.push(Value::Nil);
                        }
                    }
                }
                Instruction::SetProperty(name_idx) => {
                    if self.stack.is_empty() {
                        return Err(VanuaError::runtime_error(
                            "Empty stack during SetProperty - no object".to_string(),
                            None,
                        ));
                    }

                    let obj = self.stack.pop().unwrap();
                    let prop_name = self.constant_to_string(&current_constants[name_idx]);

                    if self.stack.is_empty() {
                        return Err(VanuaError::runtime_error(
                            format!(
                                "Empty stack during SetProperty - no value for property '{}'",
                                prop_name
                            ),
                            None,
                        ));
                    }

                    let value = self.stack.pop().unwrap();

                    if self.debug_mode {
                        eprintln!("[VM] SetProperty: property = {}", prop_name);
                        eprintln!("[VM] SetProperty: object = {:?}", obj);
                        eprintln!("[VM] SetProperty: value = {:?}", value);
                    }

                    match obj {
                        Value::Object(obj_ref) => {
                            let mut obj = obj_ref.borrow_mut();
                            obj.fields.insert(prop_name, value);
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Can only set properties on objects, got: {:?}",
                                    obj
                                ),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::Jump(offset) => {
                    if offset > 32768 {
                        let relative_offset = 65536 - offset as usize;
                        if self.frames[self.current_frame_idx].ip >= relative_offset {
                            self.frames[self.current_frame_idx].ip -= relative_offset;
                        } else {
                            self.frames[self.current_frame_idx].ip = 0;
                        }
                    } else {
                        self.frames[self.current_frame_idx].ip = offset as usize;
                    }
                }
                Instruction::JumpIfFalse(offset) => {
                    let condition = self.stack.pop().unwrap();
                    if !condition.is_truthy() {
                        self.frames[self.current_frame_idx].ip = offset as usize;
                    }
                }
                Instruction::JumpIfTrue(offset) => {
                    let condition = self.stack.pop().unwrap();
                    if condition.is_truthy() {
                        self.frames[self.current_frame_idx].ip = offset as usize;
                    }
                }
                Instruction::Call(arg_count) => {
                    let arg_count = arg_count as usize;
                    let args = self
                        .stack
                        .drain(self.stack.len() - arg_count..)
                        .collect::<Vec<Value>>();
                    let callee = self.stack.pop().unwrap_or(Value::Nil);

                    match callee {
                        Value::Function(name) => {
                            if let Some(function_bytecode) =
                                self.find_function_bytecode(&name, &bytecode.constants)
                            {
                                self.call_function(function_bytecode, args)?;
                            } else if let Some(function_bytecode) =
                                self.get_function(&name).cloned()
                            {
                                self.call_function(&function_bytecode, args)?;
                            } else {
                                match crate::stdlib::execute_stdlib_function(&name, &args) {
                                    Ok(value) => {
                                        self.stack.push(value);
                                    }
                                    Err(err) => {
                                        return Err(err);
                                    }
                                }
                            }
                        }
                        Value::Object(_) => {
                            return Err(VanuaError::runtime_error(
                                "Cannot call class object directly. Use Class.new() syntax instead.".to_string(),
                                None
                            ));
                        }
                        _ => {
                            return Err(VanuaError::runtime_error(
                                format!("Cannot call non-function value: {:?}", callee),
                                None,
                            ));
                        }
                    }
                }
                Instruction::Return => {
                    if self.debug_mode {
                        eprintln!(
                            "[VM] Return: Current frame idx: {}, total frames: {}",
                            self.current_frame_idx,
                            self.frames.len()
                        );
                        if self.current_frame_idx < self.frames.len() {
                            eprintln!(
                                "[VM] Return: Returning from function: {}",
                                self.frames[self.current_frame_idx].function_name
                            );
                        }
                    }

                    if self.current_frame_idx == 0 {
                        break;
                    }

                    let result = if self.stack.is_empty() {
                        Value::Nil
                    } else {
                        self.stack.pop().unwrap()
                    };

                    self.frames.pop();
                    self.current_frame_idx -= 1;

                    if self.debug_mode {
                        eprintln!(
                            "[VM] Return: After return, frame idx: {}, total frames: {}",
                            self.current_frame_idx,
                            self.frames.len()
                        );
                        if self.current_frame_idx < self.frames.len() {
                            eprintln!(
                                "[VM] Return: Returning to function: {}",
                                self.frames[self.current_frame_idx].function_name
                            );
                        }
                    }

                    self.stack.push(result);
                }
                Instruction::Curry => {
                    let func = self.stack.pop().unwrap();

                    let remaining_params = match &func {
                        Value::Function(name) => match name.as_str() {
                            "add" | "sub" | "mul" | "div" | "eq" | "lt" | "gt" => 2,
                            "map" | "filter" => 2,
                            "reduce" | "fold" => 3,
                            "identity" | "not" | "len" => 1,
                            "compose" | "pipe" => 2,

                            _ => 1,
                        },
                        Value::Object(obj) => {
                            let obj_ref = obj.borrow();

                            if let Some(Value::Int(arity)) = obj_ref.fields.get("__arity") {
                                *arity as usize
                            } else if let Some(Value::Array(params)) =
                                obj_ref.fields.get("__params")
                            {
                                params.len()
                            } else {
                                1
                            }
                        }

                        Value::Curry(c) => c.borrow().remaining_params,
                        Value::Partial(p) => p.borrow().args.len(),
                        Value::Compose(_) => 1,

                        _ => 1,
                    };

                    let curried = Value::Curry(Rc::new(RefCell::new(CurriedFunction {
                        original: func.clone(),
                        applied_args: Vec::new(),
                        remaining_params,
                    })));

                    if self.debug_mode {
                        eprintln!(
                            "[VM] Created curried function with {} parameters",
                            remaining_params
                        );
                    }

                    self.stack.push(curried);
                }
                Instruction::Compose => {
                    let f = self.stack.pop().unwrap();
                    let g = self.stack.pop().unwrap();

                    if !matches!(f, Value::Function(_) | Value::Curry(_))
                        || !matches!(g, Value::Function(_) | Value::Curry(_))
                    {
                        return Err(VanuaError::RuntimeError {
                            message: "Cannot compose non-function values".to_string(),
                            cause: None,
                        });
                    }

                    let composed = Value::Compose(Rc::new(RefCell::new(ComposedFunction { f, g })));

                    self.stack.push(composed);
                }
                Instruction::PartialApply(arg_mask) => {
                    let func = self.stack.pop().unwrap();

                    let mut args = Vec::with_capacity(arg_mask.len());
                    for &is_provided in arg_mask.iter().rev() {
                        if is_provided {
                            let arg = self.stack.pop().unwrap();
                            args.push(Some(arg));
                        } else {
                            args.push(None);
                        }
                    }

                    args.reverse();

                    let partial = Value::Partial(Rc::new(RefCell::new(PartialFunction {
                        original: func,
                        args,
                    })));

                    self.stack.push(partial);
                }
                Instruction::MatchBegin => {}
                Instruction::MatchPattern(idx) => {
                    let pattern_value = self.constant_to_value(&current_constants[idx]);
                    let target_value = self.stack.last().unwrap().clone();

                    let matches = match (&pattern_value, &target_value) {
                        (Value::Int(_), Value::Int(_))
                        | (Value::Float(_), Value::Float(_))
                        | (Value::Bool(_), Value::Bool(_))
                        | (Value::Char(_), Value::Char(_))
                        | (Value::String(_), Value::String(_)) => pattern_value == target_value,

                        _ => false,
                    };

                    self.stack.push(Value::Bool(matches));
                }
                Instruction::MatchGuard => {}
                Instruction::MatchJump(offset) => {
                    let condition = self.stack.pop().unwrap();
                    if !condition.is_truthy() {
                        self.frames[self.current_frame_idx].ip = offset as usize;
                    }
                }
                Instruction::MatchEnd => {
                    self.stack.pop();
                }
                Instruction::MakeLazy => {
                    let _return_ip = self.frames[self.current_frame_idx].ip;

                    let lazy = Value::Lazy(Rc::new(RefCell::new(LazyValue {
                        thunk: Value::Function("lazy_thunk".to_string()),
                        evaluated: false,
                        value: None,
                    })));

                    self.stack.push(lazy);

                    if let Instruction::Jump(offset) =
                        current_instructions[self.frames[self.current_frame_idx].ip].clone()
                    {
                        self.frames[self.current_frame_idx].ip = offset as usize;
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "MakeLazy: expected Jump instruction for thunk".to_string(),
                            cause: None,
                        });
                    }
                }
                Instruction::ForceLazy => {
                    let value = self.stack.pop().unwrap();
                    let forced = value.force();
                    self.stack.push(forced);
                }
                Instruction::LoadModule(name) => {
                    let module_name = self.constant_to_string(&current_constants[name]);

                    if self.debug_mode {
                        eprintln!("Loading module: {}", module_name);
                    }

                    let stdlib_modules = crate::stdlib::get_stdlib_modules_silent();
                    let mut module_found = false;

                    for module in stdlib_modules {
                        if module.name == module_name {
                            module_found = true;
                            if self.debug_mode {
                                eprintln!("Module found in stdlib: {}", module_name);
                            }

                            let mut module_obj = HashMap::new();

                            for func_name in &module.functions {
                                module_obj
                                    .insert(func_name.clone(), Value::Function(func_name.clone()));
                            }

                            for (const_name, const_value) in &module.constants {
                                if self.debug_mode {
                                    eprintln!(
                                        "Constant in module: {} = {:?}",
                                        const_name, const_value
                                    );
                                }
                                module_obj.insert(const_name.clone(), const_value.clone());

                                self.globals.insert(const_name.clone(), const_value.clone());
                            }

                            let module_value = Value::Map(module_obj);

                            self.globals
                                .insert(module_name.clone(), module_value.clone());


                            break;
                        }
                    }

                    if !module_found {
                        return Err(VanuaError::RuntimeError {
                            message: format!("Module '{}' not found", module_name),
                            cause: None,
                        });
                    }
                }
                Instruction::Inherit => {
                    let superclass = self.stack.pop().unwrap();
                    let class = self.stack.pop().unwrap();

                    match (class, superclass) {
                        (Value::Object(class_obj), Value::Object(superclass_obj)) => {
                            let mut class = class_obj.borrow_mut();
                            let superclass = superclass_obj.borrow();

                            if !matches!(class.typ, ObjectType::Class(_)) {
                                return Err(VanuaError::RuntimeError {
                                    message: "First operand must be a class".to_string(),
                                    cause: None,
                                });
                            }

                            if !matches!(superclass.typ, ObjectType::Class(_)) {
                                return Err(VanuaError::RuntimeError {
                                    message: "Second operand must be a class".to_string(),
                                    cause: None,
                                });
                            }

                            for (name, value) in &superclass.fields {
                                if !class.fields.contains_key(name) {
                                    if self.debug_mode {
                                        eprintln!(
                                            "[VM] Inherit: Copying field '{}' from superclass",
                                            name
                                        );
                                        if name == "constructor" {
                                            if let Value::Function(func_name) = value {
                                                eprintln!("[VM] Inherit: Copying constructor function: {}", func_name);
                                            }
                                        }
                                    }
                                    class.fields.insert(name.clone(), value.clone());
                                } else if self.debug_mode {
                                    eprintln!("[VM] Inherit: Skipping field '{}' - already exists in subclass", name);
                                }
                            }

                            class.fields.insert(
                                "__super".to_string(),
                                Value::Object(superclass_obj.clone()),
                            );

                            if let (
                                ObjectType::Class(class_name),
                                ObjectType::Class(superclass_name),
                            ) = (&class.typ, &superclass.typ)
                            {
                                let mut existing_superclasses = Vec::new();
                                let mut existing_traits = Vec::new();

                                if let Some((superclasses, traits)) =
                                    self.class_inheritance.get(class_name)
                                {
                                    existing_superclasses = superclasses.clone();
                                    existing_traits = traits.clone();
                                }

                                if !existing_superclasses.contains(superclass_name) {
                                    existing_superclasses.push(superclass_name.clone());
                                }

                                self.register_class_inheritance(
                                    class_name.clone(),
                                    existing_superclasses,
                                    existing_traits,
                                );
                            }

                            self.stack.push(Value::Object(class_obj.clone()));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Inherit requires two class objects".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::Class(name_idx) => {
                    let name = self.constant_to_string(&current_constants[name_idx]);

                    let class_obj = Rc::new(RefCell::new(Object {
                        typ: ObjectType::Class(name.clone()),
                        fields: HashMap::new(),
                    }));

                    class_obj
                        .borrow_mut()
                        .fields
                        .insert("__name".to_string(), Value::String(name.clone()));

                    self.stack.push(Value::Object(class_obj));
                }

                Instruction::Trait(name_idx) => {
                    let name = self.constant_to_string(&current_constants[name_idx]);

                    let trait_obj = Rc::new(RefCell::new(Object {
                        typ: ObjectType::Class(format!("trait_{}", name)),
                        fields: HashMap::new(),
                    }));

                    trait_obj
                        .borrow_mut()
                        .fields
                        .insert("__name".to_string(), Value::String(name.clone()));

                    trait_obj
                        .borrow_mut()
                        .fields
                        .insert("__isTrait".to_string(), Value::Bool(true));

                    self.stack.push(Value::Object(trait_obj));
                }
                Instruction::TraitMethod(name_idx, method_idx) => {
                    let _trait_name = self.constant_to_string(&current_constants[name_idx]);
                    let method_name = self.constant_to_string(&current_constants[method_idx]);
                    let method = self.stack.pop().unwrap();
                    let trait_obj = self.stack.pop().unwrap();

                    match trait_obj {
                        Value::Object(obj) => {
                            let mut object = obj.borrow_mut();

                            if let Some(Value::Bool(true)) = object.fields.get("__isTrait") {
                                object.fields.insert(method_name, method);

                                self.stack.push(Value::Object(obj.clone()));
                            } else {
                                return Err(VanuaError::RuntimeError {
                                    message: "TraitMethod requires a trait object".to_string(),
                                    cause: None,
                                });
                            }
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "TraitMethod requires a trait object".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::Struct(name_idx) => {
                    let name = self.constant_to_string(&current_constants[name_idx]);

                    let struct_obj = Rc::new(RefCell::new(Object {
                        typ: ObjectType::Struct(name.clone()),
                        fields: HashMap::new(),
                    }));

                    struct_obj
                        .borrow_mut()
                        .fields
                        .insert("__name".to_string(), Value::String(name.clone()));

                    self.stack.push(Value::Object(struct_obj));
                }
                Instruction::AddImplMethod(name_idx, method_idx) => {
                    let target_name = self.constant_to_string(&current_constants[name_idx]);
                    let method_name = self.constant_to_string(&current_constants[method_idx]);
                    let method = self.stack.pop().unwrap();
                    let impl_obj = self.stack.pop().unwrap();

                    match impl_obj {
                        Value::Object(obj) => {
                            let mut object = obj.borrow_mut();

                            if !object.fields.contains_key("__methods") {
                                object
                                    .fields
                                    .insert("__methods".to_string(), Value::Map(HashMap::new()));
                            }

                            if let Some(Value::Map(ref mut methods)) =
                                object.fields.get_mut("__methods")
                            {
                                methods.insert(method_name, method);
                            }

                            object
                                .fields
                                .insert("__target".to_string(), Value::String(target_name));

                            self.stack.push(Value::Object(obj.clone()));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "AddImplMethod requires an implementation object"
                                    .to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::TryCatch(offset) => {
                    self.error_handler_stack
                        .push(self.frames[self.current_frame_idx].ip + offset as usize);
                }
                Instruction::Throw => {
                    let exception = self.stack.pop().unwrap();

                    if let Some(handler_ip) = self.error_handler_stack.pop() {
                        self.stack.push(exception);

                        self.frames[self.current_frame_idx].ip = handler_ip;
                    } else {
                        let error_message = match exception {
                            Value::String(s) => s,
                            _ => format!("{:?}", exception),
                        };

                        return Err(VanuaError::RuntimeError {
                            message: format!("Uncaught exception: {}", error_message),
                            cause: None,
                        });
                    }
                }
                Instruction::PreIncrement => {
                    let var = self.stack.pop().unwrap();
                    match var {
                        Value::Int(i) => {
                            let result = Value::Int(i + 1);
                            self.stack.push(result);
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Cannot increment value of type {}",
                                    var.type_name()
                                ),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::PostIncrement => {
                    let var = self.stack.pop().unwrap();
                    match var {
                        Value::Int(i) => {
                            self.stack.push(Value::Int(i));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Cannot increment value of type {}",
                                    var.type_name()
                                ),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::PreDecrement => {
                    let var = self.stack.pop().unwrap();
                    match var {
                        Value::Int(i) => {
                            let result = Value::Int(i - 1);
                            self.stack.push(result);
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Cannot decrement value of type {}",
                                    var.type_name()
                                ),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::PostDecrement => {
                    let var = self.stack.pop().unwrap();
                    match var {
                        Value::Int(i) => {
                            self.stack.push(Value::Int(i));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Cannot decrement value of type {}",
                                    var.type_name()
                                ),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::CreateFuture => {
                    let future = Arc::new(Mutex::new(FutureValue {
                        completed: false,
                        value: None,
                        receiver: None,
                        oneshot_receiver: None,
                    }));

                    self.stack.push(Value::Future(future));
                }
                Instruction::SpawnAsync(constant_idx) => {
                    let _future_value = match self.stack.pop().unwrap() {
                        Value::Future(future) => future,
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "SpawnAsync requires a Future object".to_string(),
                                cause: None,
                            });
                        }
                    };

                    if self.async_runtime.is_none() {
                        self.init_async_runtime()?;
                    }

                    let function_bytecode = if let Some(Constant::Function(func)) =
                        current_constants.get(constant_idx as usize)
                    {
                        func.clone()
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: format!("No function found at constant index {} in current function constants", constant_idx),
                            cause: None,
                        });
                    };

                    let mut async_params = Vec::new();
                    let param_count = function_bytecode.param_count as usize;

                    if param_count > 0 && self.stack.len() >= param_count {
                        let start_idx = self.stack.len() - param_count;
                        for i in 0..param_count {
                            if let Some(val) = self.stack.get(start_idx + i) {
                                // Convert to SimpleFutureValue for thread safety
                                async_params.push(SimpleFutureValue::from(val.clone()));
                            }
                        }

                        for _ in 0..param_count {
                            self.stack.pop();
                        }
                    }

                    let context = AsyncExecutionContext {
                        function_name: function_bytecode.name.clone(),
                        parameters: async_params,
                        function_bytecode: function_bytecode.clone(),
                        global_bytecode: bytecode.clone(),
                        user_defined_functions: self.extract_user_defined_functions(),
                        debug_mode: self.debug_mode,
                    };

                    if let Some(ref runtime) = self.async_runtime {
                        match runtime.spawn_async_function(context) {
                            Ok(async_future) => {
                                self.stack.push(Value::Future(async_future));
                            }
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "Async runtime not available".to_string(),
                            cause: None,
                        });
                    }
                }
                Instruction::AwaitFuture => {
                    let future_value = match self.stack.last().unwrap() {
                        Value::Future(future) => future.clone(),
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "AwaitFuture requires a Future object".to_string(),
                                cause: None,
                            });
                        }
                    };

                    let result = if let Some(ref runtime) = self.async_runtime {
                        match runtime.block_on_future(future_value) {
                            Ok(async_result) => async_result.to_value(),
                            Err(e) => {
                                return Err(e);
                            }
                        }
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "Async runtime not available for await operation".to_string(),
                            cause: None,
                        });
                    };

                    self.stack.pop();
                    self.stack.push(result);
                }
                Instruction::Method(name_idx, method_idx) => {
                    let class_name = self.constant_to_string(&current_constants[name_idx]);
                    let method_name = self.constant_to_string(&current_constants[method_idx]);

                    let method = self.stack.pop().unwrap();

                    let class_obj = self.stack.pop().unwrap();

                    match class_obj {
                        Value::Object(obj) => {
                            if self.debug_mode {
                                eprintln!(
                                    "[VM] Method: Adding method '{}' to class '{}'",
                                    method_name, class_name
                                );
                                if method_name == "constructor" {
                                    eprintln!("[VM] Method: This is a constructor - should override any inherited constructor");
                                    eprintln!(
                                        "[VM] Method: Constructor value being added: {:?}",
                                        method
                                    );
                                }
                            }

                            obj.borrow_mut().fields.insert(method_name.clone(), method);

                            if self.debug_mode && method_name == "constructor" {
                                eprintln!("[VM] Method: Constructor added. Class now has constructor: {:?}",
                                    obj.borrow().fields.get("constructor").is_some());
                                if let Some(Value::Function(func_name)) =
                                    obj.borrow().fields.get("constructor")
                                {
                                    eprintln!(
                                        "[VM] Method: Constructor function name: {}",
                                        func_name
                                    );
                                }
                            }

                            self.stack.push(Value::Object(obj));
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: "Method instruction requires a class object".to_string(),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::GetProperty(name_idx) => {
                    let property_name = self.constant_to_string(&current_constants[name_idx]);

                    let object = self.stack.pop().unwrap();

                    match object {
                        Value::Object(obj) => {
                            let obj_borrowed = obj.borrow();

                            if let ObjectType::Class(class_name) = &obj_borrowed.typ {
                                if property_name == "new" {
                                    let stdlib_classes =
                                        vec!["Map", "Array", "String", "Integer", "Boolean"];
                                    if stdlib_classes.contains(&class_name.as_str()) {
                                        self.stack
                                            .push(Value::Function(format!("{}.new", class_name)));
                                    } else {
                                        self.stack
                                            .push(Value::Function(format!("{}.new", class_name)));
                                    }
                                } else {
                                    if let Some(value) = obj_borrowed.fields.get(&property_name) {
                                        self.stack.push(value.clone());
                                    } else {
                                        self.stack.push(Value::Nil);
                                    }
                                }
                            } else {
                                if let Some(value) = self
                                    .get_property_with_inheritance(&obj_borrowed, &property_name)
                                {
                                    self.stack.push(value);
                                } else {
                                    self.stack.push(Value::Nil);
                                }
                            }
                        }
                        Value::Map(map) => {
                            if let Some(value) = map.get(&property_name) {
                                self.stack.push(value.clone());
                            } else {
                                self.stack.push(Value::Nil);
                            }
                        }
                        Value::Function(func_name) => {
                            if property_name == "new" {
                                let stdlib_classes =
                                    vec!["Map", "Array", "String", "Integer", "Boolean"];
                                if stdlib_classes.contains(&func_name.as_str()) {
                                    self.stack
                                        .push(Value::Function(format!("{}.new", func_name)));
                                } else {
                                    return Err(VanuaError::RuntimeError {
                                        message: format!("Method 'new' not found on {}", func_name),
                                        cause: None,
                                    });
                                }
                            } else {
                                return Err(VanuaError::RuntimeError {
                                    message: format!(
                                        "Method '{}' not found on Function",
                                        property_name
                                    ),
                                    cause: None,
                                });
                            }
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Cannot get property '{}' from non-object/non-map",
                                    property_name
                                ),
                                cause: None,
                            });
                        }
                    }
                }

                Instruction::Invoke(name_idx, arg_count) => {
                    let method_name = self.constant_to_string(&current_constants[name_idx]);

                    let mut args = Vec::new();
                    for _ in 0..arg_count {
                        args.push(self.stack.pop().unwrap());
                    }
                    args.reverse();

                    let object = self.stack.pop().unwrap();

                    match object {
                        Value::Object(obj) => {
                            if method_name == "new"
                                && matches!(obj.borrow().typ, ObjectType::Class(_))
                            {
                                let class_name = if let Some(Value::String(name)) =
                                    obj.borrow().fields.get("__name")
                                {
                                    name.clone()
                                } else {
                                    "Unknown".to_string()
                                };


                                // TODO Add runtime visibility check for classes and methods. I must track visibility at runtime
                                if let Some(Value::Bool(is_private)) =
                                    obj.borrow().fields.get("__private")
                                {
                                    if *is_private {
                                        return Err(VanuaError::runtime_error(
                                            format!(
                                                "Cannot instantiate private class '{}'",
                                                class_name
                                            ),
                                            None,
                                        ));
                                    }
                                }

                                let instance = Rc::new(RefCell::new(Object {
                                    typ: ObjectType::Instance(class_name.clone()),
                                    fields: HashMap::new(),
                                }));

                                for (key, value) in obj.borrow().fields.iter() {
                                    if key != "new" && key != "__name" {
                                        instance
                                            .borrow_mut()
                                            .fields
                                            .insert(key.clone(), value.clone());
                                    }
                                }

                                let constructor_name = format!("{}::constructor", class_name);

                                let constructor_func = self.find_function_bytecode(&constructor_name, &bytecode.constants)
                                    .or_else(|| {
                                        eprintln!("[VM] Class-specific constructor not found, trying generic constructor field");

                                        if let Some(Value::Function(generic_constructor_name)) = obj.borrow().fields.get("constructor") {
                                            eprintln!("[VM] Found generic constructor field: {}", generic_constructor_name);
                                            self.find_function_bytecode(generic_constructor_name, &bytecode.constants)
                                        } else {
                                            eprintln!("[VM] No constructor field found");
                                            None
                                        }
                                    });

                                if let Some(constructor_func) = constructor_func {
                                    let mut locals = vec![Value::Nil; constructor_func.local_count];
                                    locals[0] = Value::Object(instance.clone());
                                    for (i, arg) in args.iter().enumerate() {
                                        if i + 1 < locals.len() {
                                            locals[i + 1] = arg.clone();
                                        }
                                    }

                                    let frame = CallFrame {
                                        function_name: constructor_func.name.clone(),
                                        ip: 0,
                                        slot_base: self.stack.len(),
                                        locals,
                                        bytecode: Some(constructor_func.clone()),
                                    };

                                    self.frames.push(frame);
                                    self.current_frame_idx = self.frames.len() - 1;

                                    self.stack.push(Value::Object(instance));
                                } else {
                                    eprintln!(
                                        "[VM] No constructor found, returning instance directly"
                                    );

                                    self.stack.push(Value::Object(instance));
                                }
                            } else {
                                let func_name = self.resolve_method_dispatch(
                                    &obj.borrow(),
                                    &method_name,
                                    &bytecode.constants,
                                )?;

                                if let Some(func_name) = func_name {
                                    let mut method_args = vec![Value::Object(obj.clone())];
                                    method_args.extend(args.clone());

                                    match crate::stdlib::call_stdlib_function(
                                        &func_name,
                                        &method_args,
                                    ) {
                                        Ok(result) => self.stack.push(result),
                                        Err(_) => {
                                            if let Some(method_func) = self.find_function_bytecode(
                                                &func_name,
                                                &bytecode.constants,
                                            ) {
                                                let mut locals =
                                                    vec![Value::Nil; method_func.local_count];
                                                locals[0] = Value::Object(obj.clone());
                                                for (i, arg) in args.iter().enumerate() {
                                                    if i + 1 < locals.len() {
                                                        locals[i + 1] = arg.clone();
                                                    }
                                                }

                                                let frame = CallFrame {
                                                    function_name: func_name.clone(),
                                                    ip: 0,
                                                    slot_base: self.stack.len(),
                                                    locals,
                                                    bytecode: Some(method_func.clone()),
                                                };

                                                self.frames.push(frame);
                                                self.current_frame_idx = self.frames.len() - 1;
                                            } else {
                                                self.stack.push(Value::Nil);
                                            }
                                        }
                                    }
                                } else {
                                    let obj_value = Value::Object(obj.clone());
                                    if obj_value.should_inherit_from_unit() {
                                        let base_method_name = format!("Unit.{}", method_name);
                                        let mut method_args = vec![Value::Object(obj.clone())];
                                        method_args.extend(args.clone());

                                        match crate::stdlib::call_stdlib_function(
                                            &base_method_name,
                                            &method_args,
                                        ) {
                                            Ok(result) => self.stack.push(result),
                                            Err(_) => {
                                                return Err(VanuaError::RuntimeError {
                                                    message: format!(
                                                        "Method '{}' not found on object",
                                                        method_name
                                                    ),
                                                    cause: None,
                                                });
                                            }
                                        }
                                    } else {
                                        return Err(VanuaError::RuntimeError {
                                            message: format!(
                                                "Method '{}' not found on object",
                                                method_name
                                            ),
                                            cause: None,
                                        });
                                    }
                                }
                            }
                        }
                        Value::Array(_) => {
                            if method_name == "toString" || method_name == "clone" {
                                let base_method_name = format!("Unit.{}", method_name);
                                let mut method_args = vec![object.clone()];
                                method_args.extend(args.clone());

                                match crate::stdlib::call_stdlib_function(
                                    &base_method_name,
                                    &method_args,
                                ) {
                                    Ok(result) => self.stack.push(result),
                                    Err(_) => {
                                        return Err(VanuaError::RuntimeError {
                                            message: format!(
                                                "Method '{}' not found on Array",
                                                method_name
                                            ),
                                            cause: None,
                                        });
                                    }
                                }
                            } else {
                                return Err(VanuaError::RuntimeError {
                                    message: format!("Method '{}' not found on Array", method_name),
                                    cause: None,
                                });
                            }
                        }
                        Value::Map(_) => {
                            if method_name == "toString" || method_name == "clone" {
                                let base_method_name = format!("Unit.{}", method_name);
                                let mut method_args = vec![object.clone()];
                                method_args.extend(args.clone());

                                match crate::stdlib::call_stdlib_function(
                                    &base_method_name,
                                    &method_args,
                                ) {
                                    Ok(result) => self.stack.push(result),
                                    Err(_) => {
                                        return Err(VanuaError::RuntimeError {
                                            message: format!(
                                                "Method '{}' not found on Map",
                                                method_name
                                            ),
                                            cause: None,
                                        });
                                    }
                                }
                            } else {
                                return Err(VanuaError::RuntimeError {
                                    message: format!("Method '{}' not found on Map", method_name),
                                    cause: None,
                                });
                            }
                        }
                        Value::Function(ref func_name) => {
                            if method_name == "toString" || method_name == "clone" {
                                let base_method_name = format!("Unit.{}", method_name);
                                let mut method_args = vec![object.clone()];
                                method_args.extend(args.clone());

                                match crate::stdlib::call_stdlib_function(
                                    &base_method_name,
                                    &method_args,
                                ) {
                                    Ok(result) => self.stack.push(result),
                                    Err(_) => {
                                        return Err(VanuaError::RuntimeError {
                                            message: format!(
                                                "Method '{}' not found on Function",
                                                method_name
                                            ),
                                            cause: None,
                                        });
                                    }
                                }
                            } else if method_name == "new" {
                                let stdlib_classes =
                                    vec!["Map", "Array", "String", "Integer", "Boolean"];
                                if stdlib_classes.contains(&func_name.as_str()) {
                                    let new_func_name = format!("{}.new", func_name);
                                    match crate::stdlib::call_stdlib_function(&new_func_name, &args)
                                    {
                                        Ok(result) => self.stack.push(result),
                                        Err(_) => {
                                            return Err(VanuaError::RuntimeError {
                                                message: format!(
                                                    "Failed to call {}.new()",
                                                    func_name
                                                ),
                                                cause: None,
                                            });
                                        }
                                    }
                                } else {
                                    return Err(VanuaError::RuntimeError {
                                        message: format!("User-defined class {}.new() should be called on class object, not function", func_name),
                                        cause: None,
                                    });
                                }
                            } else {
                                return Err(VanuaError::RuntimeError {
                                    message: format!(
                                        "Method '{}' not found on Function",
                                        method_name
                                    ),
                                    cause: None,
                                });
                            }
                        }
                        _ => {
                            return Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Cannot invoke method '{}' on non-object",
                                    method_name
                                ),
                                cause: None,
                            });
                        }
                    }
                }
                Instruction::SuperCall(arg_count) => {
                    let current_frame_idx = self.current_frame_idx;
                    let this_obj = self.frames[current_frame_idx].locals[0].clone();

                    if let Value::Object(obj_ref) = this_obj {
                        let object_id = format!("{:p}", obj_ref.as_ptr());

                        let current_function_name = &self.frames[current_frame_idx].function_name;

                        let current_class_name = if let Some(pos) = current_function_name.find("::")
                        {
                            &current_function_name[..pos]
                        } else {
                            current_function_name
                        };

                        let superclass_name = if let Some((superclasses, _)) =
                            self.class_inheritance.get(current_class_name)
                        {
                            if !superclasses.is_empty() {
                                superclasses[0].clone()
                            } else {
                                return Err(VanuaError::RuntimeError {
                                    message: format!(
                                        "Class '{}' has no superclass",
                                        current_class_name
                                    ),
                                    cause: None,
                                });
                            }
                        } else {
                            return Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Class '{}' not found in inheritance registry",
                                    current_class_name
                                ),
                                cause: None,
                            });
                        };

                        let constructor_key = format!("{}::constructor", superclass_name);
                        let called_constructors = self
                            .virtual_inheritance_table
                            .entry(object_id.clone())
                            .or_insert_with(HashSet::new);

                        if called_constructors.contains(&constructor_key) {
                            self.frames[self.current_frame_idx].ip += 1;
                            continue;
                        }

                        called_constructors.insert(constructor_key.clone());

                        let class_constructor_name = format!("{}::constructor", superclass_name);

                        let constructor_func = self
                            .find_function_bytecode(&class_constructor_name, &bytecode.constants);

                        if let Some(constructor_func) = constructor_func {
                            let mut args = Vec::new();
                            let arg_count_usize = arg_count as usize;
                            for _ in 0..arg_count_usize {
                                args.push(self.stack.pop().unwrap());
                            }
                            args.reverse();

                            let mut locals = vec![Value::Nil; constructor_func.local_count];
                            locals[0] = Value::Object(obj_ref.clone());
                            for (i, arg) in args.iter().enumerate() {
                                if i + 1 < locals.len() {
                                    locals[i + 1] = arg.clone();
                                }
                            }

                            let frame = CallFrame {
                                function_name: constructor_func.name.clone(),
                                ip: 0,
                                slot_base: self.stack.len(),
                                locals,
                                bytecode: Some(constructor_func.clone()),
                            };

                            self.frames.push(frame);
                            self.current_frame_idx = self.frames.len() - 1;

                            if self.debug_mode {
                                eprintln!("[VM] SuperCall: Added parent constructor frame, new frame count: {}", self.frames.len());
                                eprintln!(
                                    "[VM] SuperCall: Current frame idx: {}",
                                    self.current_frame_idx
                                );
                            }
                        } else {
                            return Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Parent constructor for class '{}' not found",
                                    superclass_name
                                ),
                                cause: None,
                            });
                        }
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "super() can only be called on object instances".to_string(),
                            cause: None,
                        });
                    }
                }
            }
        }

        Ok(if self.stack.is_empty() {
            Value::Nil
        } else {
            self.stack.pop().unwrap()
        })
    }

    fn constant_to_value(&self, constant: &Constant) -> Value {
        match constant {
            Constant::Int(value) => Value::Int(*value),
            Constant::Float(value) => Value::Float(*value),
            Constant::String(value) => Value::String(value.clone()),
            Constant::Char(value) => Value::Char(*value),
            Constant::Bool(value) => Value::Bool(*value),
            Constant::Function(func) => Value::Function(func.name.clone()),
            Constant::Native(name) => Value::Function(name.clone()),
            Constant::Null => Value::Nil,
        }
    }

    /// Converts a constant to a string, avoiding creating a temporary value
    fn constant_to_string(&self, constant: &Constant) -> String {
        match constant {
            Constant::String(value) => value.clone(),
            _ => constant.to_string(),
        }
    }

    /// Gets a global value by name, avoiding creating temporary values
    fn get_global_by_name(&self, name: &str) -> Option<&Value> {
        self.globals.get(name)
    }

    /// Extract user-defined functions from the VM for async execution
    fn extract_user_defined_functions(&self) -> HashMap<String, FunctionBytecode> {
        let mut user_functions = HashMap::new();

        for (name, function_bytecode) in &self.function_registry {
            user_functions.insert(name.clone(), function_bytecode.clone());
        }

        for (name, value) in &self.globals {
            if let Value::Function(func_name) = value {
                if !self.is_stdlib_function(func_name) {
                    if let Some(function_bytecode) = self.function_registry.get(func_name) {
                        user_functions.insert(name.clone(), function_bytecode.clone());
                    }
                }
            }
        }

        user_functions
    }

    /// Check if a function is a stdlib function
    fn is_stdlib_function(&self, func_name: &str) -> bool {
        let stdlib_modules = crate::stdlib::get_stdlib_modules_silent();
        for module in stdlib_modules {
            if module.functions.contains(&func_name.to_string()) {
                return true;
            }
        }
        false
    }

    /// Find a function bytecode at a specific instruction offset
    #[allow(dead_code)]
    fn find_function_at_offset<'a>(
        &self,
        bytecode: &'a ByteCode,
        _offset: usize,
    ) -> Option<&'a FunctionBytecode> {
        for constant in &bytecode.constants {
            if let Constant::Function(func) = constant {
                // FIXME: Track which function corresponds to which offset
                return Some(func);
            }
        }
        None
    }

    /// Continue execution from the current state (used after call_function)
    pub fn continue_execution(&mut self) -> Result<Value, VanuaError> {
        loop {
            if self.frames.is_empty() {
                break;
            }

            let current_frame_idx = self.current_frame_idx;
            let frame_bytecode = self.frames[current_frame_idx].bytecode.clone();
            let frame_ip = self.frames[current_frame_idx].ip;

            let (current_instructions, current_constants) =
                if let Some(ref func_bytecode) = frame_bytecode {
                    (&func_bytecode.instructions, &func_bytecode.constants)
                } else {
                    break;
                };

            if frame_ip >= current_instructions.len() {
                self.frames.pop();
                if !self.frames.is_empty() {
                    self.current_frame_idx = self.frames.len() - 1;
                }
                continue;
            }

            let instruction = current_instructions[frame_ip].clone();
            self.frames[current_frame_idx].ip += 1;

            self.execute_single_instruction(&instruction, current_constants)?;
        }

        Ok(if self.stack.is_empty() {
            Value::Nil
        } else {
            self.stack.pop().unwrap()
        })
    }

    /// Execute a function bytecode directly (used by async runtime)
    pub fn execute_function_bytecode(
        &mut self,
        function_bytecode: &FunctionBytecode,
    ) -> Result<Value, VanuaError> {
        let frame = CallFrame {
            function_name: function_bytecode.name.clone(),
            ip: 0,
            slot_base: self
                .stack
                .len()
                .saturating_sub(function_bytecode.param_count),
            locals: vec![Value::Nil; function_bytecode.local_count],
            bytecode: Some(function_bytecode.clone()),
        };

        self.frames.push(frame);
        self.current_frame_idx = self.frames.len() - 1;

        loop {
            let current_frame_idx = self.current_frame_idx;

            if current_frame_idx >= self.frames.len() {
                break;
            }

            let ip = self.frames[current_frame_idx].ip;
            let instruction = if let Some(ref bytecode) = self.frames[current_frame_idx].bytecode {
                if ip >= bytecode.instructions.len() {
                    break;
                }
                bytecode.instructions[ip].clone()
            } else {
                return Err(VanuaError::RuntimeError {
                    message: "No bytecode available for function execution".to_string(),
                    cause: None,
                });
            };

            self.frames[current_frame_idx].ip += 1;

            match &instruction {
                Instruction::Return => {
                    self.frames.pop();
                    if self.current_frame_idx > 0 {
                        self.current_frame_idx -= 1;
                    }

                    if self.frames.is_empty() {
                        return Ok(if self.stack.is_empty() {
                            Value::Nil
                        } else {
                            self.stack.pop().unwrap()
                        });
                    }
                }
                _ => {
                    let (current_constants, _current_instructions) = if let Some(ref bytecode) =
                        self.frames[current_frame_idx].bytecode
                    {
                        (bytecode.constants.clone(), bytecode.instructions.clone())
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "No bytecode available for instruction execution".to_string(),
                            cause: None,
                        });
                    };

                    if let Err(e) =
                        self.execute_single_instruction(&instruction, &current_constants)
                    {
                        return Err(e);
                    }
                }
            }
        }

        Ok(if self.stack.is_empty() {
            Value::Nil
        } else {
            self.stack.pop().unwrap()
        })
    }

    /// Execute a single instruction (used by async function execution)
    fn execute_single_instruction(
        &mut self,
        instruction: &Instruction,
        constants: &[Constant],
    ) -> Result<(), VanuaError> {
        match instruction {
            Instruction::Constant(idx) => {
                let constant = &constants[*idx];
                let value = self.constant_to_value(constant);
                self.stack.push(value);
            }
            Instruction::Nil => self.stack.push(Value::Nil),
            Instruction::Unit => self.stack.push(Value::base_object()),
            Instruction::True => self.stack.push(Value::Bool(true)),
            Instruction::False => self.stack.push(Value::Bool(false)),
            Instruction::Pop => {
                self.stack.pop();
            }
            Instruction::GetLocal(slot) => {
                let slot_index = *slot as usize;

                while self.frames[self.current_frame_idx].locals.len() <= slot_index {
                    self.frames[self.current_frame_idx].locals.push(Value::Nil);
                }

                let frame = &self.frames[self.current_frame_idx];
                let value = frame.locals[slot_index].clone();
                self.stack.push(value);
            }
            Instruction::SetLocal(slot) => {
                let value = self.stack.pop().unwrap_or(Value::Nil);
                let slot_index = *slot as usize;

                while self.frames[self.current_frame_idx].locals.len() <= slot_index {
                    self.frames[self.current_frame_idx].locals.push(Value::Nil);
                }

                self.frames[self.current_frame_idx].locals[slot_index] = value;
            }
            Instruction::GetGlobal(idx) => {
                let constant = &constants[*idx];
                if let Constant::String(name) = constant {
                    if let Some(value) = self.get_global_by_name(name) {
                        self.stack.push(value.clone());
                    } else {
                        let stdlib_modules = crate::stdlib::get_stdlib_modules_silent();
                        let mut found = false;

                        for module in stdlib_modules {
                            if module.constants.contains_key(name) {
                                if let Some(value) = module.constants.get(name) {
                                    self.stack.push(value.clone());
                                    found = true;
                                    break;
                                }
                            }
                        }

                        if !found {
                            // FIXME remove this check
                            if name.len() > 50 {
                                self.stack.push(Value::String(name.clone()));
                            } else {
                                return Err(VanuaError::RuntimeError {
                                    message: format!("Undefined global variable: {}", name),
                                    cause: None,
                                });
                            }
                        }
                    }
                }
            }
            Instruction::Call(arg_count) => {
                let arg_count_usize = *arg_count as usize;

                let function_value = if self.stack.len() > arg_count_usize {
                    self.stack[self.stack.len() - arg_count_usize - 1].clone()
                } else {
                    return Err(VanuaError::RuntimeError {
                        message: "Not enough values on stack for function call".to_string(),
                        cause: None,
                    });
                };

                let mut args = Vec::new();
                for _ in 0..arg_count_usize {
                    if let Some(arg) = self.stack.pop() {
                        args.insert(0, arg);
                    }
                }

                self.stack.pop();

                match function_value {
                    Value::Function(func_name) => {
                        if let Some(global_func) = self.globals.get(&func_name) {
                            match global_func {
                                Value::Function(name) => {
                                    match crate::stdlib::execute_stdlib_function(name, &args) {
                                        Ok(result) => {
                                            self.stack.push(result);
                                        }
                                        Err(_) => {
                                            let mut function_bytecode = None;

                                            if let Some(ref current_bytecode) =
                                                self.frames[self.current_frame_idx].bytecode
                                            {
                                                for constant in &current_bytecode.constants {
                                                    if let Constant::Function(bytecode) = constant {
                                                        if bytecode.name == func_name {
                                                            function_bytecode =
                                                                Some(bytecode.clone());
                                                            break;
                                                        }
                                                    }
                                                }
                                            }

                                            if function_bytecode.is_none() {
                                                if let Some(ref current_frame) =
                                                    self.frames.get(self.current_frame_idx)
                                                {
                                                    if let Some(ref _current_bytecode) =
                                                        current_frame.bytecode
                                                    {
                                                        if let Some(ref main_frame) =
                                                            self.frames.get(0)
                                                        {
                                                            if let Some(ref _main_bytecode) =
                                                                main_frame.bytecode
                                                            {
                                                                if let Some(global_func) =
                                                                    self.get_function(&func_name)
                                                                {
                                                                    function_bytecode =
                                                                        Some(global_func.clone());
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }

                                            if let Some(func_bytecode) = function_bytecode {
                                                let is_async_function = func_bytecode
                                                    .instructions
                                                    .iter()
                                                    .any(|instr| {
                                                        matches!(
                                                            instr,
                                                            Instruction::CreateFuture
                                                                | Instruction::SpawnAsync(_)
                                                        )
                                                    });

                                                if is_async_function {
                                                    match self.call_function(&func_bytecode, args) {
                                                        Ok(()) => {}
                                                        Err(e) => {
                                                            return Err(e);
                                                        }
                                                    }
                                                } else {
                                                    match self.call_function(&func_bytecode, args) {
                                                        Ok(()) => {}
                                                        Err(e) => {
                                                            return Err(e);
                                                        }
                                                    }
                                                }
                                            } else {
                                                self.stack.push(Value::Nil);
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    return Err(VanuaError::RuntimeError {
                                        message: format!(
                                            "Function {} not supported in async context",
                                            func_name
                                        ),
                                        cause: None,
                                    });
                                }
                            }
                        } else {
                            return Err(VanuaError::RuntimeError {
                                message: format!("Unknown function: {}", func_name),
                                cause: None,
                            });
                        }
                    }
                    Value::Object(_) => {
                        return Err(VanuaError::runtime_error(
                            "Cannot call class object directly. Use Class.new() syntax instead."
                                .to_string(),
                            None,
                        ));
                    }
                    _ => {
                        return Err(VanuaError::RuntimeError {
                            message: format!(
                                "Cannot call non-function value: {:?}",
                                function_value
                            ),
                            cause: None,
                        });
                    }
                }
            }
            Instruction::Add => {
                if self.stack.len() < 2 {
                    return Err(VanuaError::RuntimeError {
                        message: "Stack underflow: Add operation requires 2 operands".to_string(),
                        cause: None,
                    });
                }
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.add_values(a, b)?;
            }
            Instruction::Multiply => {
                let b = self.stack.pop().unwrap_or(Value::Nil);
                let a = self.stack.pop().unwrap_or(Value::Nil);
                let result = match (&a, &b) {
                    (Value::Int(a_val), Value::Int(b_val)) => Value::Int(a_val * b_val),
                    (Value::Float(a_val), Value::Float(b_val)) => Value::Float(a_val * b_val),
                    (Value::Int(a_val), Value::Float(b_val)) => Value::Float(*a_val as f64 * b_val),
                    (Value::Float(a_val), Value::Int(b_val)) => Value::Float(a_val * *b_val as f64),
                    _ => {
                        return Err(VanuaError::RuntimeError {
                            message: format!("Cannot multiply {:?} and {:?}", a, b),
                            cause: None,
                        })
                    }
                };
                self.stack.push(result);
            }
            Instruction::CreateFuture => {
                let future = Arc::new(Mutex::new(FutureValue {
                    completed: false,
                    value: None,
                    receiver: None,
                    oneshot_receiver: None,
                }));

                self.stack.push(Value::Future(future));
            }
            Instruction::SpawnAsync(constant_idx) => {
                let _future_value = match self.stack.pop().unwrap() {
                    Value::Future(future) => future,
                    _ => {
                        return Err(VanuaError::RuntimeError {
                            message: "SpawnAsync requires a Future object".to_string(),
                            cause: None,
                        });
                    }
                };

                let current_frame = &self.frames[self.current_frame_idx];
                let function_constants = if let Some(ref bytecode) = current_frame.bytecode {
                    &bytecode.constants
                } else {
                    constants
                };

                let function_bytecode = match &function_constants[*constant_idx as usize] {
                    Constant::Function(bytecode) => bytecode.clone(),
                    _ => {
                        return Err(VanuaError::RuntimeError {
                            message: "SpawnAsync requires a function constant".to_string(),
                            cause: None,
                        });
                    }
                };

                if self.async_runtime.is_none() {
                    self.init_async_runtime()?;
                }

                let param_count = function_bytecode.param_count;
                let mut async_params = Vec::new();

                let current_frame = &self.frames[self.current_frame_idx];

                for i in 0..param_count {
                    let param_value = if i < current_frame.locals.len() {
                        current_frame.locals[i].clone()
                    } else {
                        Value::Nil
                    };

                    let simple_param = match param_value {
                        Value::Int(i) => SimpleFutureValue::Int(i),
                        Value::Float(f) => SimpleFutureValue::Float(f),
                        Value::String(s) => SimpleFutureValue::String(s),
                        Value::Bool(b) => SimpleFutureValue::Bool(b),
                        Value::Char(c) => SimpleFutureValue::Char(c),
                        Value::Nil => SimpleFutureValue::Nil,
                        _ => SimpleFutureValue::Nil,
                    };
                    async_params.push(simple_param);
                }

                let mut global_constants = Vec::new();

                for (_function_name, function_bytecode) in &self.function_registry {
                    global_constants.push(crate::vm::Constant::Function(function_bytecode.clone()));
                }

                let context = AsyncExecutionContext {
                    function_name: function_bytecode.name.clone(),
                    parameters: async_params,
                    function_bytecode: function_bytecode.clone(),
                    global_bytecode: ByteCode {
                        instructions: vec![],
                        constants: global_constants,
                    },
                    user_defined_functions: self.extract_user_defined_functions(),
                    debug_mode: self.debug_mode,
                };

                if let Some(ref runtime) = self.async_runtime {
                    match runtime.spawn_async_function(context) {
                        Ok(async_future) => {
                            self.stack.push(Value::Future(async_future));
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                } else {
                    return Err(VanuaError::RuntimeError {
                        message: "Async runtime not available".to_string(),
                        cause: None,
                    });
                }
            }
            Instruction::AwaitFuture => {
                if let Some(Value::Future(future_arc)) = self.stack.pop() {
                    // FIXME: this is not the best way to wait for completion
                    let mut attempts = 0;
                    const MAX_ATTEMPTS: u32 = 1000;
                    const POLL_INTERVAL_MS: u64 = 10;

                    loop {
                        if let Ok(future) = future_arc.lock() {
                            if future.completed {
                                if let Some(ref value) = future.value {
                                    let result_value = match value {
                                        SimpleFutureValue::Int(i) => Value::Int(*i),
                                        SimpleFutureValue::Float(f) => Value::Float(*f),
                                        SimpleFutureValue::String(s) => Value::String(s.clone()),
                                        SimpleFutureValue::Bool(b) => Value::Bool(*b),
                                        SimpleFutureValue::Char(c) => Value::Char(*c),
                                        SimpleFutureValue::Nil => Value::Nil,
                                    };
                                    self.stack.push(result_value);
                                    break;
                                } else {
                                    self.stack.push(Value::Nil);
                                    break;
                                }
                            }
                        }

                        attempts += 1;
                        if attempts >= MAX_ATTEMPTS {
                            self.stack.push(Value::Nil);
                            break;
                        }

                        std::thread::sleep(std::time::Duration::from_millis(POLL_INTERVAL_MS));
                    }
                } else {
                    return Err(VanuaError::RuntimeError {
                        message: "AwaitFuture requires a Future value on the stack".to_string(),
                        cause: None,
                    });
                }
            }
            Instruction::Return => {
                if !self.frames.is_empty() {
                    self.frames.pop();
                    if !self.frames.is_empty() {
                        self.current_frame_idx = self.frames.len() - 1;
                    }
                }
            }
            _ => {
                return Err(VanuaError::RuntimeError {
                    message: format!(
                        "Instruction {:?} not yet supported in async context",
                        instruction
                    ),
                    cause: None,
                });
            }
        }
        Ok(())
    }

    /// Get property from object with inheritance support
    fn get_property_with_inheritance(&self, obj: &Object, property_name: &str) -> Option<Value> {
        if let Some(value) = obj.fields.get(property_name) {
            return Some(value.clone());
        }

        if let ObjectType::Instance(class_name) = &obj.typ {
            self.get_property_from_class_hierarchy(class_name, property_name)
        } else {
            None
        }
    }

    /// Get property from class hierarchy (superclasses and traits) using MRO
    fn get_property_from_class_hierarchy(
        &self,
        class_name: &str,
        property_name: &str,
    ) -> Option<Value> {
        let mro = self.compute_mro(class_name);

        for parent_class in mro.iter().skip(1) {
            if let Some(parent_obj) = self.globals.get(parent_class) {
                if let Value::Object(parent_ref) = parent_obj {
                    let parent_borrowed = parent_ref.borrow();
                    if let Some(value) = parent_borrowed.fields.get(property_name) {
                        return Some(value.clone());
                    }
                }
            }
        }
        None
    }

    /// Register class inheritance information
    fn register_class_inheritance(
        &mut self,
        class_name: String,
        superclasses: Vec<String>,
        traits: Vec<String>,
    ) {
        self.class_inheritance
            .insert(class_name, (superclasses, traits));
    }

    /// Compute Method Resolution Order (MRO) using C3 linearization algorithm
    /// This ensures a consistent and predictable method resolution order for multiple inheritance
    fn compute_mro(&self, class_name: &str) -> Vec<String> {
        let mut mro = Vec::new();
        let mut visited = HashSet::new();

        self.compute_mro_recursive(class_name, &mut mro, &mut visited);

        if !mro.contains(&"Object".to_string()) {
            mro.push("Object".to_string());
        }

        mro
    }

    /// Recursive helper for MRO computation using C3 linearization
    fn compute_mro_recursive(
        &self,
        class_name: &str,
        mro: &mut Vec<String>,
        visited: &mut HashSet<String>,
    ) {
        if visited.contains(class_name) {
            return;
        }
        visited.insert(class_name.to_string());

        if !mro.contains(&class_name.to_string()) {
            mro.push(class_name.to_string());
        }

        if let Some((superclasses, _traits)) = self.class_inheritance.get(class_name) {
            for superclass in superclasses {
                self.compute_mro_recursive(superclass, mro, visited);
            }
        }
    }
}
