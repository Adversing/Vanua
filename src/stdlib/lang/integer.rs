use crate::error::VanuaError;
use crate::vm::{Object, ObjectType, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn get_functions() -> Vec<String> {
    vec![
        "Integer.new".to_string(),
        "Integer.from".to_string(),
        "Integer.toString".to_string(),
        "Integer.compareTo".to_string(),
        "Integer.parseInt".to_string(),
        "Integer.MAX_VALUE".to_string(),
        "Integer.MIN_VALUE".to_string(),
        "int_to_string".to_string(),
    ]
}

pub fn integer_new(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() > 1 {
        return Err(VanuaError::RuntimeError {
            message: "Integer.new: expected 0 or 1 arguments".to_string(),
            cause: None,
        });
    }

    let int_value = if args.is_empty() {
        0
    } else {
        match &args[0] {
            Value::Int(i) => *i,
            Value::Float(f) => *f as i64,
            Value::Bool(b) => {
                if *b {
                    1
                } else {
                    0
                }
            }
            Value::String(s) => s.parse::<i64>().unwrap_or(0),
            _ => 0,
        }
    };

    let mut fields = HashMap::new();

    fields.insert("value".to_string(), Value::Int(int_value));

    fields.insert(
        "toString".to_string(),
        Value::Function("Integer.toString".to_string()),
    );
    fields.insert(
        "compareTo".to_string(),
        Value::Function("Integer.compareTo".to_string()),
    );

    fields.insert(
        "MAX_VALUE".to_string(),
        Value::Function("Integer.MAX_VALUE".to_string()),
    );
    fields.insert(
        "MIN_VALUE".to_string(),
        Value::Function("Integer.MIN_VALUE".to_string()),
    );

    let object = Object {
        typ: ObjectType::Instance("Integer".to_string()),
        fields,
    };

    Ok(Value::Object(Rc::new(RefCell::new(object))))
}

pub fn integer_from(args: &[Value]) -> Result<Value, VanuaError> {
    integer_new(args)
}

pub fn integer_to_string(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Integer.toString: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Integer" => {
                    if let Some(Value::Int(i)) = obj.fields.get("value") {
                        crate::stdlib::lang::string::string_new(&[Value::String(i.to_string())])
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Integer.toString: integer value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Integer.toString: expected Integer object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Integer.toString: expected Integer object".to_string(),
            cause: None,
        }),
    }
}

pub fn integer_compare_to(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Integer.compareTo: expected 2 arguments (this, other)".to_string(),
            cause: None,
        });
    }

    let this_value = match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Integer" => {
                    if let Some(Value::Int(i)) = obj.fields.get("value") {
                        *i
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "Integer.compareTo: integer value not found".to_string(),
                            cause: None,
                        });
                    }
                }
                _ => {
                    return Err(VanuaError::RuntimeError {
                        message: "Integer.compareTo: expected Integer object".to_string(),
                        cause: None,
                    })
                }
            }
        }
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Integer.compareTo: expected Integer object".to_string(),
                cause: None,
            })
        }
    };

    let other_value = match &args[1] {
        Value::Int(i) => *i,
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Integer" => {
                    if let Some(Value::Int(i)) = obj.fields.get("value") {
                        *i
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "Integer.compareTo: integer value not found in other"
                                .to_string(),
                            cause: None,
                        });
                    }
                }
                _ => {
                    return Err(VanuaError::RuntimeError {
                        message: "Integer.compareTo: expected Integer object as other".to_string(),
                        cause: None,
                    })
                }
            }
        }
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Integer.compareTo: expected Integer object or integer as other"
                    .to_string(),
                cause: None,
            })
        }
    };

    Ok(Value::Int(match this_value.cmp(&other_value) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }))
}

pub fn integer_parse_int(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Integer.parseInt: expected 1 argument (string)".to_string(),
            cause: None,
        });
    }

    let str_value = match &args[0] {
        Value::String(s) => s.clone(),
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "String" => {
                    if let Some(Value::String(s)) = obj.fields.get("value") {
                        s.clone()
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "Integer.parseInt: string value not found".to_string(),
                            cause: None,
                        });
                    }
                }
                _ => {
                    return Err(VanuaError::RuntimeError {
                        message: "Integer.parseInt: expected String object or string".to_string(),
                        cause: None,
                    })
                }
            }
        }
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Integer.parseInt: expected String object or string".to_string(),
                cause: None,
            })
        }
    };

    match str_value.parse::<i64>() {
        Ok(i) => integer_new(&[Value::Int(i)]),
        Err(_) => Err(VanuaError::RuntimeError {
            message: format!("Integer.parseInt: cannot parse '{}' as integer", str_value),
            cause: None,
        }),
    }
}

pub fn integer_max_value(_args: &[Value]) -> Result<Value, VanuaError> {
    Ok(Value::Int(i64::MAX))
}

pub fn integer_min_value(_args: &[Value]) -> Result<Value, VanuaError> {
    Ok(Value::Int(i64::MIN))
}

pub fn int_to_string(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "int_to_string: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => Ok(Value::String(i.to_string())),
        _ => Err(VanuaError::RuntimeError {
            message: "int_to_string: expected Int value".to_string(),
            cause: None,
        }),
    }
}
