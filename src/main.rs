use clap::{Parser, Subcommand};
use miette::{IntoDiagnostic, Result};
use rustyline::DefaultEditor;
use std::collections::HashMap;
use std::io::{self, Write};
use std::path::PathBuf;
use vanua_lib::error::VanuaError;
use vanua_lib::{version, Compiler};

#[derive(Parser)]
#[command(name = "vanua")]
#[command(author = "Adversing")]
#[command(version = version())]
#[command(about = "Vanua Programming Language Compiler and VM", long_about = None)]
struct Cli {
    #[arg(short, long)]
    debug: bool,

    #[arg(short = 'D', long)]
    diagnostics: bool,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Run {
        #[arg(value_name = "FILE")]
        file: PathBuf,

        #[arg(short, long, value_name = "PHASE")]
        stop_after: Option<String>,

        #[arg(last = true, value_name = "ARGS")]
        args: Vec<String>,
    },
    Repl {
        #[arg(short, long, value_name = "FILE")]
        preload: Option<PathBuf>,

        #[arg(short, long)]
        show_types: bool,
    },
    Compile {
        #[arg(value_name = "FILE")]
        file: PathBuf,

        #[arg(short, long, value_name = "OUTPUT")]
        output: Option<PathBuf>,
    },
    Debug {
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let debug_mode = cli.debug;
    let diagnostics = cli.diagnostics;

    match cli.command {
        Some(Commands::Run {
            file,
            stop_after,
            args,
        }) => {
            let mut compiler = Compiler::from_file(&file).into_diagnostic()?;
            if debug_mode {
                compiler.debug_mode = true;
            }

            if let Some(phase) = stop_after {
                match phase.to_lowercase().as_str() {
                    "lexing" | "lex" => {
                        println!("Stopping after lexing phase");
                        let tokens =
                            vanua_lib::lexer::tokenize(&compiler.source).into_diagnostic()?;
                        println!(
                            "Lexical analysis successful. {} tokens generated.",
                            tokens.len()
                        );
                        return Ok(());
                    }
                    "parsing" | "parse" => {
                        println!("Stopping after parsing phase");
                        let tokens =
                            vanua_lib::lexer::tokenize(&compiler.source).into_diagnostic()?;
                        let ast = vanua_lib::parser::parse(tokens).into_diagnostic()?;
                        println!("Parsing successful. AST generated.");
                        if diagnostics {
                            println!("{:#?}", ast);
                        }
                        return Ok(());
                    }
                    "typechecking" | "typecheck" => {
                        println!("Stopping after type checking phase");
                        let tokens =
                            vanua_lib::lexer::tokenize(&compiler.source).into_diagnostic()?;
                        let ast = vanua_lib::parser::parse(tokens).into_diagnostic()?;
                        let typed_ast = vanua_lib::typechecker::typecheck(ast).into_diagnostic()?;
                        println!("Type checking successful.");
                        if diagnostics {
                            println!("{:#?}", typed_ast);
                        }
                        return Ok(());
                    }
                    "codegen" => {
                        println!("Stopping after code generation phase");
                        let bytecode = compiler.compile().into_diagnostic()?;
                        println!(
                            "Code generation successful. {} instructions generated.",
                            bytecode.instructions.len()
                        );
                        if diagnostics {
                            println!("{:#?}", bytecode);
                        }
                        return Ok(());
                    }
                    _ => {
                        eprintln!("Unknown phase: {}. Valid phases: lexing, parsing, typechecking, codegen", phase);
                        std::process::exit(1);
                    }
                }
            }

            match compiler.compile_and_run_with_args(args) {
                Ok(_result) => Ok(()),
                Err(e) => {
                    if diagnostics {
                        print_detailed_error(&e, &compiler.source);
                    }
                    Err(e).into_diagnostic()?
                }
            }
        }
        Some(Commands::Repl {
            preload,
            show_types,
        }) => {
            println!("Vanua Programming Language REPL v{}", version());
            println!("Type '.help' for available commands or 'exit' to quit");

            // repl environment startup
            let mut globals = HashMap::<String, vanua_lib::vm::Value>::new();

            if let Some(file_path) = preload {
                let mut compiler = Compiler::from_file(&file_path).into_diagnostic()?;
                if debug_mode {
                    compiler.debug_mode = true;
                    println!("Preloading file: {}", file_path.display());
                }

                match compiler.compile_and_run() {
                    Ok(_) => {
                        println!("File preloaded successfully");
                    }
                    Err(e) => {
                        eprintln!("Error preloading file: {}", e);
                        if diagnostics {
                            print_detailed_error(&e, &compiler.source);
                        }
                    }
                }
            }

            let mut rl = DefaultEditor::new().into_diagnostic()?;
            let _ = rl.load_history("vanua_history.txt");

            let mut multiline_input = String::new();
            let mut in_multiline = false;

            loop {
                let prompt = if in_multiline { "... " } else { "vanua> " };
                let readline = rl.readline(prompt);

                match readline {
                    Ok(line) => {
                        if line.trim() == "exit" {
                            break;
                        }

                        if line.trim() == ".help" {
                            println!("REPL Commands:");
                            println!("  .help       Display this help message");
                            println!("  .clear      Clear the screen");
                            println!("  .multiline  Start/end multiline input mode");
                            println!("  .types      Toggle type information display");
                            println!("  .info       Show environment information");
                            println!("  .reset      Reset the REPL environment");
                            println!("  .load FILE  Load and execute a file");
                            println!("  exit        Exit the REPL");
                            continue;
                        } else if line.trim() == ".clear" {
                            print!("\x1B[2J\x1B[1;1H"); // screen clearing sequence
                            continue;
                        } else if line.trim() == ".multiline" {
                            in_multiline = !in_multiline;
                            if in_multiline {
                                println!(
                                    "Multiline mode enabled. Type '.multiline' again to execute."
                                );
                                multiline_input.clear();
                            } else {
                                if !multiline_input.is_empty() {
                                    let _ = rl.add_history_entry(&multiline_input);
                                    execute_code(
                                        &multiline_input,
                                        debug_mode,
                                        diagnostics,
                                        show_types,
                                    );
                                    multiline_input.clear();
                                }
                            }
                            continue;
                        } else if line.trim() == ".types" {
                            let show_types = !show_types;
                            println!(
                                "Type information display: {}",
                                if show_types { "enabled" } else { "disabled" }
                            );
                            continue;
                        } else if line.trim() == ".info" {
                            println!("Vanua Environment Information:");
                            println!("  Version:     {}", version());
                            println!(
                                "  Debug mode:  {}",
                                if debug_mode { "enabled" } else { "disabled" }
                            );
                            println!(
                                "  Diagnostics: {}",
                                if diagnostics { "enabled" } else { "disabled" }
                            );
                            println!(
                                "  Types:       {}",
                                if show_types { "visible" } else { "hidden" }
                            );
                            continue;
                        } else if line.trim() == ".reset" {
                            globals.clear();
                            println!("REPL environment reset");
                            continue;
                        } else if line.trim().starts_with(".load ") {
                            let file_path = line.trim().strip_prefix(".load ").unwrap().trim();
                            let path = PathBuf::from(file_path);

                            if !path.exists() {
                                eprintln!("Error: File not found: {}", file_path);
                                continue;
                            }

                            let mut compiler = match Compiler::from_file(&path) {
                                Ok(c) => c,
                                Err(e) => {
                                    eprintln!("Error loading file: {}", e);
                                    continue;
                                }
                            };

                            if debug_mode {
                                compiler.debug_mode = true;
                            }

                            match compiler.compile_and_run() {
                                Ok(_) => println!("File loaded successfully"),
                                Err(e) => {
                                    eprintln!("Error executing file: {}", e);
                                    if diagnostics {
                                        print_detailed_error(&e, &compiler.source);
                                    }
                                }
                            }
                            continue;
                        }

                        if in_multiline {
                            multiline_input.push_str(&line);
                            multiline_input.push('\n');
                            continue;
                        }

                        if !line.trim().is_empty() {
                            if let Err(e) = rl.add_history_entry(line.as_str()) {
                                eprintln!("Warning: Could not add history entry: {}", e);
                            }

                            execute_code(&line, debug_mode, diagnostics, show_types);
                        }
                    }
                    Err(_) => break,
                }
            }

            let _ = rl.save_history("vanua_history.txt");
            Ok(())
        }
        Some(Commands::Compile { file, output }) => {
            let mut compiler = Compiler::from_file(&file).into_diagnostic()?;
            if debug_mode {
                compiler.debug_mode = true;
            }

            let bytecode = match compiler.compile() {
                Ok(b) => b,
                Err(e) => {
                    if diagnostics {
                        print_detailed_error(&e, &compiler.source);
                    }
                    return Err(e).into_diagnostic()?;
                }
            };

            let output_path = match output {
                Some(path) => path,
                None => {
                    let mut path = file.clone();
                    path.set_extension("vbc");
                    path
                }
            };

            let serialized = serde_json::to_string(&bytecode).into_diagnostic()?;
            let bytecode_size = serialized.len();
            std::fs::write(&output_path, serialized).into_diagnostic()?;
            println!(
                "Compilation successful! Output written to {}",
                output_path.display()
            );

            if debug_mode {
                println!("Bytecode statistics:");
                println!("  Instructions: {}", bytecode.instructions.len());
                println!("  Constants: {}", bytecode.constants.len());
                println!("  Bytecode size: {} bytes", bytecode_size);
            }

            Ok(())
        }
        Some(Commands::Debug { file }) => {
            run_interactive_debugger(&file, debug_mode, diagnostics)?;
            Ok(())
        }
        None => {
            println!("Vanua Programming Language Compiler v{}", version());
            println!("Use --help for usage information");
            Ok(())
        }
    }
}

/// Execute a code snippet in the REPL environment
fn execute_code(code: &str, debug_mode: bool, diagnostics: bool, show_types: bool) {
    let mut compiler = Compiler::new(code.to_string(), None);
    compiler.debug_mode = debug_mode;

    match compiler.compile_and_run() {
        Ok(result) => {
            if show_types {
                println!("=> {} : {}", result, result.type_name());
            } else {
                println!("=> {}", result);
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            if diagnostics {
                print_detailed_error(&e, code);
            }
        }
    }
}

// TODO: refactor all the error checking/printing/handling/suggestions.

/// Print a detailed error message with context and suggestions
fn print_detailed_error(error: &VanuaError, source: &str) {
    let red = "\x1B[31m";
    let green = "\x1B[32m";
    let _yellow = "\x1B[33m";
    let blue = "\x1B[34m";
    let _magenta = "\x1B[35m";
    let cyan = "\x1B[36m";
    let bold = "\x1B[1m";
    let reset = "\x1B[0m";

    let (line, column, error_type) = match error {
        VanuaError::LexError {
            line,
            column,
            message: _,
        } => (*line, *column, format!("{}Lexical Error{}", bold, reset)),
        VanuaError::ParseError {
            line,
            column,
            message: _,
        } => (*line, *column, format!("{}Syntax Error{}", bold, reset)),
        VanuaError::TypeError {
            line,
            column,
            expected,
            found,
            ..
        } => (
            *line,
            *column,
            format!(
                "{}Type Error{}: expected {}{}{}, found {}{}{}",
                bold, reset, green, expected, reset, red, found, reset
            ),
        ),
        VanuaError::UndefinedVariable { line, column, name } => (
            *line,
            *column,
            format!(
                "{}Undefined Variable{}: '{}{}{}'",
                bold, reset, cyan, name, reset
            ),
        ),
        VanuaError::UndefinedFunction { line, column, name } => (
            *line,
            *column,
            format!(
                "{}Undefined Function{}: '{}{}{}'",
                bold, reset, cyan, name, reset
            ),
        ),
        VanuaError::UndefinedClass { line, column, name } => (
            *line,
            *column,
            format!(
                "{}Undefined Class{}: '{}{}{}'",
                bold, reset, cyan, name, reset
            ),
        ),
        VanuaError::CodegenError { line, column, .. } => (
            *line,
            *column,
            format!("{}Code Generation Error{}", bold, reset),
        ),
        VanuaError::PatternMatchError { line, column, .. } => (
            *line,
            *column,
            format!("{}Pattern Matching Error{}", bold, reset),
        ),
        VanuaError::RuntimeError { .. } => return print_runtime_error(error, source),
        _ => return, // no line available
    };

    let lines: Vec<&str> = source.lines().collect();

    let error_length = match error {
        VanuaError::LexError { .. } => 1,
        VanuaError::ParseError { .. } => 1,
        VanuaError::UndefinedVariable { name, .. } => name.len(),
        VanuaError::UndefinedFunction { name, .. } => name.len(),
        VanuaError::UndefinedClass { name, .. } => name.len(),
        VanuaError::TypeError { .. } => 1,
        VanuaError::CodegenError { .. } => 1,
        VanuaError::PatternMatchError { .. } => 1,
        _ => 1,
    };

    eprintln!(
        "\n{}=== {} at line {}, column {} ==={}",
        bold, error_type, line, column, reset
    );

    eprintln!("{}Code context:{}", bold, reset);

    let start_line = if line > 3 { line - 3 } else { 1 };
    for i in start_line..line {
        if i <= lines.len() {
            eprintln!(
                "  {}{}{}| {}",
                if i == line - 1 { blue } else { "" },
                i,
                reset,
                lines[i - 1]
            );
        }
    }

    if line <= lines.len() {
        let line_text = lines[line - 1];
        eprintln!("  {}{}{}| {}", red, line, reset, line_text);

        let mut marker = String::with_capacity(column + 6);
        marker.push_str("  ");
        marker.push_str("  ");
        marker.push_str("| ");

        if column > 0 {
            marker.extend(std::iter::repeat(' ').take(column.saturating_sub(1)));
        }

        marker.push_str(&format!("{}{}{}", red, "^", reset));
        if error_length > 1 {
            marker.push_str(&format!("{}{}{}", red, "~".repeat(error_length - 1), reset));
        }

        eprintln!("{}", marker);
    }

    for i in (line + 1)..=(line + 2).min(lines.len()) {
        eprintln!(
            "  {}{}{}| {}",
            if i == line + 1 { blue } else { "" },
            i,
            reset,
            lines[i - 1]
        );
    }

    eprintln!(
        "\n{}Error details:{} {}{}{}",
        bold,
        reset,
        red,
        error.to_string(),
        reset
    );

    match error {
        VanuaError::TypeError {
            expected,
            found,
            message,
            ..
        } => {
            print_type_error_help(expected, found, message);
        }
        VanuaError::UndefinedVariable { name, .. } => {
            print_undefined_variable_help(name, source);
        }
        VanuaError::ParseError { message, .. } => {
            print_parse_error_help(message, lines, line);
        }
        _ => {}
    }

    eprintln!();
}

// TODO: Refactor
/// Print detailed help for type errors
fn print_type_error_help(expected: &str, found: &str, message: &str) {
    let yellow = "\x1B[33m";
    let cyan = "\x1B[36m";
    let bold = "\x1B[1m";
    let reset = "\x1B[0m";

    eprintln!("\n{}Suggestion:{} {}", bold, reset, message);

    // Common type error suggestions
    if expected.contains("Int") && found.contains("String") {
        eprintln!(
            "{}Try parsing the string to an integer:{} {}parseInt(yourString){}",
            yellow, reset, cyan, reset
        );
    } else if expected.contains("Float") && found.contains("Int") {
        eprintln!(
            "{}Try converting the integer to a float:{} {}yourInt as Float{}",
            yellow, reset, cyan, reset
        );
    } else if expected.contains("Bool") {
        eprintln!(
            "{}Use a boolean condition:{} {}true, false, or a comparison{}",
            yellow, reset, cyan, reset
        );
    } else if expected.contains("Function") {
        eprintln!("{}Make sure you're passing a function, not calling it:{} {}use 'functionName' instead of 'functionName()'{}", 
                 yellow, reset, cyan, reset);
    }
}

/// Print detailed help for undefined variable errors
fn print_undefined_variable_help(name: &str, source: &str) {
    let yellow = "\x1B[33m";
    let green = "\x1B[32m";
    let bold = "\x1B[1m";
    let reset = "\x1B[0m";

    eprintln!(
        "\n{}Suggestion:{} Check if '{}' is declared before use.",
        bold, reset, name
    );

    let lines: Vec<&str> = source.lines().collect();
    let mut similar_vars = Vec::new();

    for line in lines {
        for word in line.split(|c: char| !c.is_alphanumeric() && c != '_') {
            let word = word.trim();
            if !word.is_empty() && word != name && is_similar(name, word) {
                similar_vars.push(word);
            }
        }
    }

    similar_vars.sort();
    similar_vars.dedup();

    if !similar_vars.is_empty() {
        eprintln!("{}Did you mean one of these?{}", yellow, reset);
        for (i, var) in similar_vars.iter().take(3).enumerate() {
            eprintln!("  {}. {}{}{}", i + 1, green, var, reset);
        }
    }

    eprintln!(
        "{}Remember to declare variables with 'val' or 'var' before using them.{}",
        yellow, reset
    );
}

/// Print detailed help for parse errors
fn print_parse_error_help(message: &str, lines: Vec<&str>, line: usize) {
    let yellow = "\x1B[33m";
    let _cyan = "\x1B[36m";
    let bold = "\x1B[1m";
    let reset = "\x1B[0m";

    if message.contains("expected") {
        if message.contains("';'") {
            eprintln!(
                "{}Suggestion:{} {}Add a semicolon ';' at the end of the statement{}",
                bold, reset, yellow, reset
            );
        } else if message.contains("')'") {
            eprintln!(
                "{}Suggestion:{} {}Missing closing parenthesis ')'{}",
                bold, reset, yellow, reset
            );
        } else if message.contains("'}'") {
            eprintln!(
                "{}Suggestion:{} {}Missing closing brace '}}'{}",
                bold, reset, yellow, reset
            );
        } else if message.contains("'{'") {
            eprintln!(
                "{}Suggestion:{} {}Missing opening brace '{{'{}",
                bold, reset, yellow, reset
            );
        }
    }

    if line <= lines.len() {
        let line_text = lines[line - 1];
        let (parens, braces, brackets) = count_delimiters(line_text);

        if parens != 0 || braces != 0 || brackets != 0 {
            eprintln!("\n{}Unbalanced delimiters in this line:{}", bold, reset);
            if parens > 0 {
                eprintln!(
                    "  {}Missing {} closing parenthesis ')'{}",
                    yellow, parens, reset
                );
            } else if parens < 0 {
                eprintln!(
                    "  {}Extra {} closing parenthesis ')'{}",
                    yellow, -parens, reset
                );
            }

            if braces > 0 {
                eprintln!(
                    "  {}Missing {} closing braces '}}'{}",
                    yellow, braces, reset
                );
            } else if braces < 0 {
                eprintln!("  {}Extra {} closing braces '}}'{}", yellow, -braces, reset);
            }

            if brackets > 0 {
                eprintln!(
                    "  {}Missing {} closing brackets ']'{}",
                    yellow, brackets, reset
                );
            } else if brackets < 0 {
                eprintln!(
                    "  {}Extra {} closing brackets ']'{}",
                    yellow, -brackets, reset
                );
            }
        }
    }
}

/// Print runtime error details
fn print_runtime_error(error: &VanuaError, _source: &str) {
    let red = "\x1B[31m";
    let bold = "\x1B[1m";
    let reset = "\x1B[0m";

    eprintln!("\n{}=== Runtime Error ==={}", bold, reset);
    eprintln!("{}{}{}", red, error, reset);

    if let VanuaError::RuntimeError { message: _, cause } = error {
        if let Some(cause_error) = cause {
            eprintln!("\n{}Caused by:{}", bold, reset);
            eprintln!("  {}{}{}", red, cause_error, reset);
        }
    }
}

/// Determine if two strings are similar
fn is_similar(s1: &str, s2: &str) -> bool {
    if (s1.len() as i32 - s2.len() as i32).abs() > 2 {
        return false;
    }

    if s1.len() <= 10 && s2.len() <= 10 {
        let distance = levenshtein_distance(s1, s2);
        return distance <= 2;
    }

    let s1_lower = s1.to_lowercase();
    let s2_lower = s2.to_lowercase();

    if s1_lower.starts_with(&s2_lower) || s2_lower.starts_with(&s1_lower) {
        return true;
    }

    false
}

// TODO move to utils
/// Calculate Levenshtein edit distance between two strings
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let s1: Vec<char> = s1.chars().collect();
    let s2: Vec<char> = s2.chars().collect();

    let m = s1.len();
    let n = s2.len();

    if m == 0 {
        return n;
    }
    if n == 0 {
        return m;
    }

    let mut dp = vec![vec![0; n + 1]; m + 1];

    for i in 0..=m {
        dp[i][0] = i;
    }
    for j in 0..=n {
        dp[0][j] = j;
    }

    for i in 1..=m {
        for j in 1..=n {
            let cost = if s1[i - 1] == s2[j - 1] { 0 } else { 1 };
            dp[i][j] = (dp[i - 1][j] + 1)
                .min(dp[i][j - 1] + 1)
                .min(dp[i - 1][j - 1] + cost);
        }
    }

    dp[m][n]
}

fn count_delimiters(s: &str) -> (i32, i32, i32) {
    let mut parens = 0;
    let mut braces = 0;
    let mut brackets = 0;

    for c in s.chars() {
        match c {
            '(' => parens += 1,
            ')' => parens -= 1,
            '{' => braces += 1,
            '}' => braces -= 1,
            '[' => brackets += 1,
            ']' => brackets -= 1,
            _ => {}
        }
    }

    (parens, braces, brackets)
}

/// Run an interactive debugger session
fn run_interactive_debugger(file: &PathBuf, debug_mode: bool, diagnostics: bool) -> Result<()> {
    if !file.exists() {
        eprintln!("Error: File not found: {}", file.display());
        std::process::exit(1);
    }

    let mut compiler = Compiler::from_file(file).into_diagnostic()?;
    compiler.debug_mode = debug_mode;

    println!("Compiling program for debugging...");

    // program compilation
    let bytecode = match compiler.compile() {
        Ok(b) => b,
        Err(e) => {
            if diagnostics {
                print_detailed_error(&e, &compiler.source);
            }
            return Err(e).into_diagnostic()?;
        }
    };

    println!("Program compiled successfully. Starting debugger...");
    println!("Type 'help' for available commands.");

    // VM instantiation
    let mut vm = vanua_lib::vm::VirtualMachine::new();
    let _ = vm.init_async_runtime();
    vm.set_globals(vanua_lib::stdlib::init_stdlib());

    let mut line = String::new();
    loop {
        print!("debug> ");
        io::stdout().flush().unwrap();
        line.clear();
        io::stdin().read_line(&mut line).unwrap();

        let command = line.trim();

        match command {
            "help" => {
                println!("Available commands:");
                println!("  run                 - Run the program");
                println!("  step                - Execute next instruction");
                println!("  info                - Show program information");
                println!("  bytecode            - Display bytecode instructions");
                println!("  stack               - Show current stack");
                println!("  globals             - Show global variables");
                println!("  break <line>        - Set breakpoint at line");
                println!("  exit/quit           - Exit debugger");
            }
            "exit" | "quit" => {
                println!("Exiting debugger.");
                break;
            }
            "run" => {
                println!("Running program...");
                match vm.execute(bytecode.clone()) {
                    Ok(_result) => {
                        println!("Program executed successfully.");
                    }
                    Err(e) => {
                        println!("Runtime error: {}", e);
                    }
                }
            }
            "info" => {
                println!("Program information:");
                println!("  Instructions: {}", bytecode.instructions.len());
                println!("  Constants: {}", bytecode.constants.len());
                println!("  Source file: {}", file.display());
            }
            "bytecode" => {
                println!("Bytecode instructions:");
                for (i, instruction) in bytecode.instructions.iter().enumerate() {
                    println!("{:4}: {:?}", i, instruction);
                }
            }
            "stack" => {
                println!("Current stack:");
                let stack = vm.get_stack();
                if stack.is_empty() {
                    println!("  (empty)");
                } else {
                    for (i, value) in stack.iter().enumerate() {
                        println!("  {}: {}", i, value);
                    }
                }
            }
            "globals" => {
                println!("Global variables:");
                let globals = vm.get_globals();
                if globals.is_empty() {
                    println!("  (none)");
                } else {
                    for (name, value) in globals.iter() {
                        println!("  {}: {}", name, value);
                    }
                }
            }
            "step" => {
                println!("Single step execution not yet implemented");
                println!("Use 'run' to execute the entire program");
            }
            _ => {
                if command.is_empty() {
                    continue;
                }
                println!("Unknown command: {}", command);
                println!("Type 'help' for available commands.");
            }
        }
    }

    Ok(())
}
