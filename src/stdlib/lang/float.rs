use crate::error::VanuaError;
use crate::vm::{Object, ObjectType, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn get_functions() -> Vec<String> {
    vec![
        "Float.new".to_string(),
        "Float.from".to_string(),
        "Float.toString".to_string(),
        "Float.compareTo".to_string(),
        "Float.parseFloat".to_string(),
        "Float.MAX_VALUE".to_string(),
        "Float.MIN_VALUE".to_string(),
        "Float.POSITIVE_INFINITY".to_string(),
        "Float.NEGATIVE_INFINITY".to_string(),
        "Float.NaN".to_string(),
        "float_to_string_primitive".to_string(),
    ]
}

pub fn float_new(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() > 1 {
        return Err(VanuaError::RuntimeError {
            message: "Float.new: expected 0 or 1 arguments".to_string(),
            cause: None,
        });
    }

    let float_value = if args.is_empty() {
        0.0
    } else {
        match &args[0] {
            Value::Float(f) => *f,
            Value::Int(i) => *i as f64,
            Value::Bool(b) => {
                if *b {
                    1.0
                } else {
                    0.0
                }
            }
            Value::String(s) => s.parse::<f64>().unwrap_or(0.0),
            _ => 0.0,
        }
    };

    let mut fields = HashMap::new();

    fields.insert("value".to_string(), Value::Float(float_value));

    fields.insert(
        "toString".to_string(),
        Value::Function("Float.toString".to_string()),
    );
    fields.insert(
        "compareTo".to_string(),
        Value::Function("Float.compareTo".to_string()),
    );

    fields.insert(
        "MAX_VALUE".to_string(),
        Value::Function("Float.MAX_VALUE".to_string()),
    );
    fields.insert(
        "MIN_VALUE".to_string(),
        Value::Function("Float.MIN_VALUE".to_string()),
    );
    fields.insert(
        "POSITIVE_INFINITY".to_string(),
        Value::Function("Float.POSITIVE_INFINITY".to_string()),
    );
    fields.insert(
        "NEGATIVE_INFINITY".to_string(),
        Value::Function("Float.NEGATIVE_INFINITY".to_string()),
    );
    fields.insert("NaN".to_string(), Value::Function("Float.NaN".to_string()));

    let object = Object {
        typ: ObjectType::Instance("Float".to_string()),
        fields,
    };

    Ok(Value::Object(Rc::new(RefCell::new(object))))
}

pub fn float_from(args: &[Value]) -> Result<Value, VanuaError> {
    float_new(args)
}

pub fn float_to_string(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Float.toString: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Float" => {
                    if let Some(Value::Float(f)) = obj.fields.get("value") {
                        crate::stdlib::lang::string::string_new(&[Value::String(f.to_string())])
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Float.toString: float value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Float.toString: expected Float object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Float.toString: expected Float object".to_string(),
            cause: None,
        }),
    }
}

pub fn float_compare_to(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Float.compareTo: expected 2 arguments (this, other)".to_string(),
            cause: None,
        });
    }

    let this_value = match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Float" => {
                    if let Some(Value::Float(f)) = obj.fields.get("value") {
                        *f
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "Float.compareTo: float value not found".to_string(),
                            cause: None,
                        });
                    }
                }
                _ => {
                    return Err(VanuaError::RuntimeError {
                        message: "Float.compareTo: expected Float object".to_string(),
                        cause: None,
                    })
                }
            }
        }
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Float.compareTo: expected Float object".to_string(),
                cause: None,
            })
        }
    };

    let other_value = match &args[1] {
        Value::Float(f) => *f,
        Value::Int(i) => *i as f64,
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Float" => {
                    if let Some(Value::Float(f)) = obj.fields.get("value") {
                        *f
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "Float.compareTo: float value not found in other".to_string(),
                            cause: None,
                        });
                    }
                }
                _ => {
                    return Err(VanuaError::RuntimeError {
                        message: "Float.compareTo: expected Float object as other".to_string(),
                        cause: None,
                    })
                }
            }
        }
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Float.compareTo: expected Float object or number as other".to_string(),
                cause: None,
            })
        }
    };

    Ok(Value::Int(if this_value < other_value {
        -1
    } else if this_value > other_value {
        1
    } else {
        0
    }))
}

pub fn float_parse_float(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Float.parseFloat: expected 1 argument (string)".to_string(),
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
                            message: "Float.parseFloat: string value not found".to_string(),
                            cause: None,
                        });
                    }
                }
                _ => {
                    return Err(VanuaError::RuntimeError {
                        message: "Float.parseFloat: expected String object or string".to_string(),
                        cause: None,
                    })
                }
            }
        }
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Float.parseFloat: expected String object or string".to_string(),
                cause: None,
            })
        }
    };

    match str_value.parse::<f64>() {
        Ok(f) => float_new(&[Value::Float(f)]),
        Err(_) => Err(VanuaError::RuntimeError {
            message: format!("Float.parseFloat: cannot parse '{}' as float", str_value),
            cause: None,
        }),
    }
}

pub fn float_max_value(_args: &[Value]) -> Result<Value, VanuaError> {
    Ok(Value::Float(f64::MAX))
}

pub fn float_min_value(_args: &[Value]) -> Result<Value, VanuaError> {
    Ok(Value::Float(f64::MIN))
}

pub fn float_positive_infinity(_args: &[Value]) -> Result<Value, VanuaError> {
    Ok(Value::Float(f64::INFINITY))
}

pub fn float_negative_infinity(_args: &[Value]) -> Result<Value, VanuaError> {
    Ok(Value::Float(f64::NEG_INFINITY))
}

pub fn float_nan(_args: &[Value]) -> Result<Value, VanuaError> {
    Ok(Value::Float(f64::NAN))
}

pub fn float_to_string_primitive(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "float_to_string: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Float(f) => Ok(Value::String(f.to_string())),
        _ => Err(VanuaError::RuntimeError {
            message: "float_to_string: expected Float value".to_string(),
            cause: None,
        }),
    }
}
