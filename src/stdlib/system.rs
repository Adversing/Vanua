use crate::error::VanuaError;
use crate::vm::Value;
use std::collections::HashMap;
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Get the current UNIX timestamp in seconds
pub fn time(_args: &[Value]) -> Result<Value, VanuaError> {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => Ok(Value::Float(n.as_secs_f64())),
        Err(e) => Err(VanuaError::RuntimeError {
            message: format!("time: system time error: {}", e),
            cause: None,
        }),
    }
}

/// Exit the program with an optional status code
pub fn exit(args: &[Value]) -> Result<Value, VanuaError> {
    let code = match args.len() {
        0 => 0,
        1 => match &args[0] {
            Value::Int(n) => {
                if *n < 0 || *n > 255 {
                    return Err(VanuaError::RuntimeError {
                        message: "exit: status code must be between 0 and 255".to_string(),
                        cause: None,
                    });
                }
                *n as i32
            }
            _ => {
                return Err(VanuaError::RuntimeError {
                    message: "exit: status code must be an integer".to_string(),
                    cause: None,
                })
            }
        },
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "exit: expected 0 or 1 arguments".to_string(),
                cause: None,
            })
        }
    };

    std::process::exit(code);
}

/// Sleep for a specified number of milliseconds
pub fn sleep(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "sleep: expected 1 argument".to_string(),
            cause: None,
        });
    }

    let ms = match &args[0] {
        Value::Int(n) => {
            if *n < 0 {
                return Err(VanuaError::RuntimeError {
                    message: "sleep: milliseconds cannot be negative".to_string(),
                    cause: None,
                });
            }
            *n as u64
        }
        Value::Float(n) => {
            if *n < 0.0 {
                return Err(VanuaError::RuntimeError {
                    message: "sleep: milliseconds cannot be negative".to_string(),
                    cause: None,
                });
            }
            *n as u64
        }
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "sleep: argument must be a number".to_string(),
                cause: None,
            })
        }
    };

    thread::sleep(Duration::from_millis(ms));
    Ok(Value::Nil)
}

/// Get information about the operating system
pub fn os_info(_args: &[Value]) -> Result<Value, VanuaError> {
    let mut info = HashMap::new();

    info.insert(
        "family".to_string(),
        Value::String(std::env::consts::FAMILY.to_string()),
    );
    info.insert(
        "os".to_string(),
        Value::String(std::env::consts::OS.to_string()),
    );
    info.insert(
        "arch".to_string(),
        Value::String(std::env::consts::ARCH.to_string()),
    );

    Ok(Value::Map(info))
}

/// Get environment variables
pub fn env_vars(args: &[Value]) -> Result<Value, VanuaError> {
    match args.len() {
        0 => {
            let mut env_map = HashMap::new();
            for (key, value) in std::env::vars() {
                env_map.insert(key, Value::String(value));
            }
            Ok(Value::Map(env_map))
        }
        1 => match &args[0] {
            Value::String(name) => match std::env::var(name) {
                Ok(value) => Ok(Value::String(value)),
                Err(_) => Ok(Value::Nil),
            },
            _ => Err(VanuaError::RuntimeError {
                message: "env_vars: variable name must be a string".to_string(),
                cause: None,
            }),
        },
        _ => Err(VanuaError::RuntimeError {
            message: "env_vars: expected 0 or 1 arguments".to_string(),
            cause: None,
        }),
    }
}

/// Get current working directory
pub fn cwd(_args: &[Value]) -> Result<Value, VanuaError> {
    match std::env::current_dir() {
        Ok(path) => match path.to_str() {
            Some(path_str) => Ok(Value::String(path_str.to_string())),
            None => Err(VanuaError::RuntimeError {
                message: "cwd: path contains invalid Unicode".to_string(),
                cause: None,
            }),
        },
        Err(e) => Err(VanuaError::RuntimeError {
            message: format!("cwd: failed to get current directory: {}", e),
            cause: None,
        }),
    }
}

pub fn get_module() -> super::StdlibModule {
    super::StdlibModule {
        name: "system".to_string(),
        functions: vec![
            "time".to_string(),
            "exit".to_string(),
            "sleep".to_string(),
            "os_info".to_string(),
            "env_vars".to_string(),
            "cwd".to_string(),
        ],
        constants: HashMap::new(),
    }
}
