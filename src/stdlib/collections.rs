use crate::error::VanuaError;
use crate::vm::Value;
use std::collections::HashMap;

/// Get the length of a collection
pub fn len(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "len: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        Value::Array(a) => Ok(Value::Int(a.len() as i64)),
        Value::Map(m) => Ok(Value::Int(m.len() as i64)),
        _ => Err(VanuaError::RuntimeError {
            message: "len: argument must be a collection (string, array, or map)".to_string(),
            cause: None,
        }),
    }
}

/// Add an element to the end of an array, append to a string, or add a key-value pair to a map
pub fn push(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() < 2 {
        return Err(VanuaError::RuntimeError {
            message: "push: expected at least 2 arguments".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Array(array) => {
            let mut new_array = array.clone();
            new_array.push(args[1].clone());
            Ok(Value::Array(new_array))
        }
        Value::String(string) => {
            let mut new_string = string.clone();
            match &args[1] {
                Value::String(s) => {
                    new_string.push_str(s);
                    Ok(Value::String(new_string))
                }
                Value::Char(c) => {
                    new_string.push(*c);
                    Ok(Value::String(new_string))
                }
                other => {
                    new_string.push_str(&other.to_string());
                    Ok(Value::String(new_string))
                }
            }
        }
        Value::Map(map) => {
            if args.len() != 3 {
                return Err(VanuaError::RuntimeError {
                    message: "push: expected 3 arguments when pushing to a map (map, key, value)"
                        .to_string(),
                    cause: None,
                });
            }

            let mut new_map = map.clone();
            match &args[1] {
                Value::String(key) => {
                    new_map.insert(key.clone(), args[2].clone());
                    Ok(Value::Map(new_map))
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "push: map key must be a string".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "push: first argument must be an array, string, or map".to_string(),
            cause: None,
        }),
    }
}

/// Remove and return the last element from an array
pub fn pop(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "pop: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Array(array) => {
            let mut new_array = array.clone();
            if let Some(value) = new_array.pop() {
                Ok(value)
            } else {
                Ok(Value::Nil)
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "pop: argument must be an array".to_string(),
            cause: None,
        }),
    }
}

/// Insert a key-value pair into a map
pub fn insert(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 3 {
        return Err(VanuaError::RuntimeError {
            message: "insert: expected 3 arguments".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Map(map) => {
            let mut new_map = map.clone();

            match &args[1] {
                Value::String(key) => {
                    new_map.insert(key.clone(), args[2].clone());
                    Ok(Value::Map(new_map))
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "insert: key must be a string".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "insert: first argument must be a map".to_string(),
            cause: None,
        }),
    }
}

/// Remove a key-value pair from a map and return the removed value
pub fn remove(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "remove: expected 2 arguments".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Map(map) => {
            let mut new_map = map.clone();

            match &args[1] {
                Value::String(key) => {
                    if let Some(value) = new_map.remove(key) {
                        Ok(value)
                    } else {
                        Ok(Value::Nil)
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "remove: key must be a string".to_string(),
                    cause: None,
                }),
            }
        }
        Value::Array(array) => {
            let mut new_array = array.clone();

            match &args[1] {
                Value::Int(index) => {
                    if *index < 0 || *index >= array.len() as i64 {
                        return Err(VanuaError::RuntimeError {
                            message: format!(
                                "remove: index {} out of bounds for array of length {}",
                                index,
                                array.len()
                            ),
                            cause: None,
                        });
                    }

                    let value = new_array.remove(*index as usize);
                    Ok(value)
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "remove: index must be an integer".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "remove: first argument must be a map or array".to_string(),
            cause: None,
        }),
    }
}

/// Check if a collection contains a value
pub fn contains(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "contains: expected 2 arguments".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::String(string) => match &args[1] {
            Value::String(substring) => Ok(Value::Bool(string.contains(substring))),
            Value::Char(c) => Ok(Value::Bool(string.contains(*c))),
            _ => Err(VanuaError::RuntimeError {
                message:
                    "contains: second argument must be a string or character when checking a string"
                        .to_string(),
                cause: None,
            }),
        },
        Value::Array(array) => Ok(Value::Bool(array.contains(&args[1]))),
        Value::Map(map) => match &args[1] {
            Value::String(key) => Ok(Value::Bool(map.contains_key(key))),
            _ => Err(VanuaError::RuntimeError {
                message: "contains: key must be a string when checking a map".to_string(),
                cause: None,
            }),
        },
        _ => Err(VanuaError::RuntimeError {
            message: "contains: first argument must be a collection (string, array, or map)"
                .to_string(),
            cause: None,
        }),
    }
}

pub fn get_module() -> super::StdlibModule {
    super::StdlibModule {
        name: "collections".to_string(),
        functions: vec![
            "len".to_string(),
            "push".to_string(),
            "pop".to_string(),
            "insert".to_string(),
            "remove".to_string(),
            "contains".to_string(),
        ],
        constants: HashMap::new(),
    }
}
