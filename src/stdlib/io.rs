use crate::error::VanuaError;
use crate::vm::Value;
use std::fs::{self, OpenOptions};
use std::io::{self, Write};
use std::path::Path;

/// Print a value to stdout without a newline
pub fn print(args: &[Value]) -> Result<Value, VanuaError> {
    use std::io::{self, Write};
    use std::sync::Mutex;

    static OUTPUT_MUTEX: Mutex<()> = Mutex::new(());

    if args.is_empty() {
        return Err(VanuaError::RuntimeError {
            message: "print: expected at least 1 argument".to_string(),
            cause: None,
        });
    }

    let _lock = OUTPUT_MUTEX.lock().unwrap();

    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }

        match arg {
            Value::String(s) => print!("{}", s),
            _ => print!("{}", arg),
        }
    }

    io::stdout().flush().map_err(|e| VanuaError::RuntimeError {
        message: format!("Failed to flush stdout: {}", e),
        cause: None,
    })?;

    Ok(Value::Nil)
}

/// Print a value to stdout with a newline
pub fn println(args: &[Value]) -> Result<Value, VanuaError> {
    use std::io::{self, Write};
    use std::sync::Mutex;

    static OUTPUT_MUTEX: Mutex<()> = Mutex::new(());

    let _lock = OUTPUT_MUTEX.lock().unwrap();

    if args.is_empty() {
        println!();
    } else {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }

            match arg {
                Value::String(s) => print!("{}", s),
                _ => print!("{}", arg),
            }
        }
        println!();
    }

    io::stdout().flush().map_err(|e| VanuaError::RuntimeError {
        message: format!("Failed to flush stdout: {}", e),
        cause: None,
    })?;

    Ok(Value::Nil)
}

/// Read a line from stdin
pub fn read_line(_args: &[Value]) -> Result<Value, VanuaError> {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .map_err(|e| VanuaError::RuntimeError {
            message: format!("Failed to read from stdin: {}", e),
            cause: None,
        })?;

    if input.ends_with('\n') {
        input.pop();
        if input.ends_with('\r') {
            input.pop();
        }
    }

    Ok(Value::String(input))
}

/// Read an integer from stdin
pub fn read_int(_args: &[Value]) -> Result<Value, VanuaError> {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .map_err(|e| VanuaError::RuntimeError {
            message: format!("Failed to read from stdin: {}", e),
            cause: None,
        })?;

    let trimmed = input.trim();
    match trimmed.parse::<i64>() {
        Ok(i) => Ok(Value::Int(i)),
        Err(_) => Err(VanuaError::RuntimeError {
            message: format!("Failed to parse '{}' as an integer", trimmed),
            cause: None,
        }),
    }
}

/// Read a floating-point number from stdin
pub fn read_float(_args: &[Value]) -> Result<Value, VanuaError> {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .map_err(|e| VanuaError::RuntimeError {
            message: format!("Failed to read from stdin: {}", e),
            cause: None,
        })?;

    let trimmed = input.trim();
    match trimmed.parse::<f64>() {
        Ok(f) => Ok(Value::Float(f)),
        Err(_) => Err(VanuaError::RuntimeError {
            message: format!("Failed to parse '{}' as a floating-point number", trimmed),
            cause: None,
        }),
    }
}

/// Read the entire contents of a file
pub fn read_file(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "read_file: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::String(path) => {
            let contents = fs::read_to_string(path).map_err(|e| VanuaError::RuntimeError {
                message: format!("read_file: failed to read file '{}': {}", path, e),
                cause: None,
            })?;

            Ok(Value::String(contents))
        }
        _ => Err(VanuaError::RuntimeError {
            message: "read_file: path must be a string".to_string(),
            cause: None,
        }),
    }
}

/// Write content to a file
pub fn write_file(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "write_file: expected 2 arguments".to_string(),
            cause: None,
        });
    }

    match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => {
            fs::write(path, content).map_err(|e| VanuaError::RuntimeError {
                message: format!("write_file: failed to write to file '{}': {}", path, e),
                cause: None,
            })?;

            Ok(Value::Nil)
        }
        (_, _) => Err(VanuaError::RuntimeError {
            message: "write_file: path and content must be strings".to_string(),
            cause: None,
        }),
    }
}

/// Append content to a file
pub fn append_file(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "append_file: expected 2 arguments".to_string(),
            cause: None,
        });
    }

    match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => {
            let mut file = OpenOptions::new()
                .create(true)
                .append(true)
                .open(path)
                .map_err(|e| VanuaError::RuntimeError {
                    message: format!("append_file: failed to open file '{}': {}", path, e),
                    cause: None,
                })?;

            file.write_all(content.as_bytes())
                .map_err(|e| VanuaError::RuntimeError {
                    message: format!("append_file: failed to write to file '{}': {}", path, e),
                    cause: None,
                })?;

            Ok(Value::Nil)
        }
        (_, _) => Err(VanuaError::RuntimeError {
            message: "append_file: path and content must be strings".to_string(),
            cause: None,
        }),
    }
}

/// Check if a file exists
pub fn file_exists(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "file_exists: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::String(path) => {
            let exists = Path::new(path).exists();
            Ok(Value::Bool(exists))
        }
        _ => Err(VanuaError::RuntimeError {
            message: "file_exists: path must be a string".to_string(),
            cause: None,
        }),
    }
}

/// Delete a file
pub fn delete_file(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "delete_file: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::String(path) => {
            fs::remove_file(path).map_err(|e| VanuaError::RuntimeError {
                message: format!("delete_file: failed to delete file '{}': {}", path, e),
                cause: None,
            })?;

            Ok(Value::Nil)
        }
        _ => Err(VanuaError::RuntimeError {
            message: "delete_file: path must be a string".to_string(),
            cause: None,
        }),
    }
}

pub fn get_module() -> super::StdlibModule {
    use std::collections::HashMap;

    super::StdlibModule {
        name: "io".to_string(),
        functions: vec![
            "print".to_string(),
            "println".to_string(),
            "readLine".to_string(),
            "readInt".to_string(),
            "readFloat".to_string(),
            "read_file".to_string(),
            "write_file".to_string(),
            "append_file".to_string(),
            "file_exists".to_string(),
            "delete_file".to_string(),
        ],
        constants: HashMap::new(),
    }
}
