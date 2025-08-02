use crate::error::VanuaError;
use crate::vm::{Object, ObjectType, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn get_functions() -> Vec<String> {
    vec![
        "String.new".to_string(),
        "String.from".to_string(),
        "String.length".to_string(),
        "String.charAt".to_string(),
        "String.substring".to_string(),
        "String.indexOf".to_string(),
        "String.contains".to_string(),
        "String.toUpperCase".to_string(),
        "String.toLowerCase".to_string(),
        "String.replace".to_string(),
        "String.trim".to_string(),
        "String.split".to_string(),
    ]
}

pub fn string_new(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() > 1 {
        return Err(VanuaError::RuntimeError {
            message: "String.new: expected 0 or 1 arguments".to_string(),
            cause: None,
        });
    }

    let string_value = if args.is_empty() {
        String::new()
    } else {
        match &args[0] {
            Value::String(s) => s.clone(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Char(c) => c.to_string(),
            Value::Nil => "null".to_string(),
            _ => format!("{}", args[0]),
        }
    };

    let mut fields = HashMap::new();

    fields.insert("value".to_string(), Value::String(string_value.clone()));

    fields.insert(
        "length".to_string(),
        Value::Function("String.length".to_string()),
    );
    fields.insert(
        "charAt".to_string(),
        Value::Function("String.charAt".to_string()),
    );
    fields.insert(
        "substring".to_string(),
        Value::Function("String.substring".to_string()),
    );
    fields.insert(
        "indexOf".to_string(),
        Value::Function("String.indexOf".to_string()),
    );
    fields.insert(
        "contains".to_string(),
        Value::Function("String.contains".to_string()),
    );
    fields.insert(
        "toUpperCase".to_string(),
        Value::Function("String.toUpperCase".to_string()),
    );
    fields.insert(
        "toLowerCase".to_string(),
        Value::Function("String.toLowerCase".to_string()),
    );
    fields.insert(
        "replace".to_string(),
        Value::Function("String.replace".to_string()),
    );
    fields.insert(
        "trim".to_string(),
        Value::Function("String.trim".to_string()),
    );
    fields.insert(
        "split".to_string(),
        Value::Function("String.split".to_string()),
    );

    let object = Object {
        typ: ObjectType::Instance("String".to_string()),
        fields,
    };

    Ok(Value::Object(Rc::new(RefCell::new(object))))
}

pub fn string_from(args: &[Value]) -> Result<Value, VanuaError> {
    string_new(args)
}

pub fn string_length(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "String.length: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        Ok(Value::Int(s.len() as i64))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.length: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.length: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.length: expected String object".to_string(),
            cause: None,
        }),
    }
}

pub fn string_char_at(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "String.charAt: expected 2 arguments (this, index)".to_string(),
            cause: None,
        });
    }

    let index = match &args[1] {
        Value::Int(i) => *i as usize,
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "String.charAt: index must be an integer".to_string(),
                cause: None,
            })
        }
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        let chars: Vec<char> = s.chars().collect();
                        if index < chars.len() {
                            Ok(Value::Char(chars[index]))
                        } else {
                            Err(VanuaError::RuntimeError {
                                message: format!(
                                    "String.charAt: index {} out of bounds for string of length {}",
                                    index,
                                    chars.len()
                                ),
                                cause: None,
                            })
                        }
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.charAt: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.charAt: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.charAt: expected String object".to_string(),
            cause: None,
        }),
    }
}

pub fn string_substring(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 3 {
        return Err(VanuaError::RuntimeError {
            message: "String.substring: expected 3 arguments (this, start, end)".to_string(),
            cause: None,
        });
    }

    let start = match &args[1] {
        Value::Int(i) => *i as usize,
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "String.substring: start must be an integer".to_string(),
                cause: None,
            })
        }
    };

    let end = match &args[2] {
        Value::Int(i) => *i as usize,
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "String.substring: end must be an integer".to_string(),
                cause: None,
            })
        }
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        let chars: Vec<char> = s.chars().collect();

                        if start > end || end > chars.len() {
                            return Err(VanuaError::RuntimeError {
                                message: format!("String.substring: invalid range (start: {}, end: {}) for string of length {}", 
                                                start, end, chars.len()),
                                cause: None,
                            });
                        }

                        let substring: String = chars[start..end].iter().collect();

                        string_new(&[Value::String(substring)])
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.substring: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.substring: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.substring: expected String object".to_string(),
            cause: None,
        }),
    }
}

pub fn string_index_of(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "String.indexOf: expected 2 arguments (this, substring)".to_string(),
            cause: None,
        });
    }

    let substring = match &args[1] {
        Value::String(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "String.indexOf: substring must be a string or char".to_string(),
                cause: None,
            })
        }
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        match s.find(&substring) {
                            Some(index) => Ok(Value::Int(index as i64)),
                            None => Ok(Value::Int(-1)),
                        }
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.indexOf: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.indexOf: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.indexOf: expected String object".to_string(),
            cause: None,
        }),
    }
}

pub fn string_contains(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "String.contains: expected 2 arguments (this, substring)".to_string(),
            cause: None,
        });
    }

    let substring = match &args[1] {
        Value::String(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "String.contains: substring must be a string or char".to_string(),
                cause: None,
            })
        }
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        Ok(Value::Bool(s.contains(&substring)))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.contains: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.contains: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.contains: expected String object".to_string(),
            cause: None,
        }),
    }
}

pub fn string_to_upper_case(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "String.toUpperCase: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        string_new(&[Value::String(s.to_uppercase())])
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.toUpperCase: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.toUpperCase: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.toUpperCase: expected String object".to_string(),
            cause: None,
        }),
    }
}

pub fn string_to_lower_case(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "String.toLowerCase: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        string_new(&[Value::String(s.to_lowercase())])
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.toLowerCase: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.toLowerCase: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.toLowerCase: expected String object".to_string(),
            cause: None,
        }),
    }
}

pub fn string_replace(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 3 {
        return Err(VanuaError::RuntimeError {
            message: "String.replace: expected 3 arguments (this, old, new)".to_string(),
            cause: None,
        });
    }

    let old_str = match &args[1] {
        Value::String(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "String.replace: old substring must be a string or char".to_string(),
                cause: None,
            })
        }
    };

    let new_str = match &args[2] {
        Value::String(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "String.replace: new substring must be a string or char".to_string(),
                cause: None,
            })
        }
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        let replaced = s.replace(&old_str, &new_str);
                        string_new(&[Value::String(replaced)])
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.replace: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.replace: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.replace: expected String object".to_string(),
            cause: None,
        }),
    }
}

pub fn string_trim(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "String.trim: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        string_new(&[Value::String(s.trim().to_string())])
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.trim: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.trim: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.trim: expected String object".to_string(),
            cause: None,
        }),
    }
}

pub fn string_split(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "String.split: expected 2 arguments (this, delimiter)".to_string(),
            cause: None,
        });
    }

    let delimiter = match &args[1] {
        Value::String(s) => s.clone(),
        Value::Char(c) => c.to_string(),
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "String.split: delimiter must be a string or char".to_string(),
                cause: None,
            })
        }
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        let parts: Vec<&str> = s.split(&delimiter).collect();

                        let mut string_parts = Vec::new();
                        for part in parts {
                            let part_obj = string_new(&[Value::String(part.to_string())])?;
                            string_parts.push(part_obj);
                        }

                        Ok(Value::Array(string_parts))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "String.split: string value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "String.split: expected String object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "String.split: expected String object".to_string(),
            cause: None,
        }),
    }
}
