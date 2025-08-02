use crate::error::VanuaError;
use crate::vm::{Object, ObjectType, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn get_functions() -> Vec<String> {
    vec![
        "Boolean.new".to_string(),
        "Boolean.from".to_string(),
        "Boolean.toString".to_string(),
        "Boolean.valueOf".to_string(),
        "Boolean.TRUE".to_string(),
        "Boolean.FALSE".to_string(),
        "is_not_null".to_string(),
        "bool_to_string".to_string(),
    ]
}

pub fn boolean_new(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() > 1 {
        return Err(VanuaError::RuntimeError {
            message: "Boolean.new: expected 0 or 1 arguments".to_string(),
            cause: None,
        });
    }

    let bool_value = if args.is_empty() {
        false
    } else {
        match &args[0] {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty() && s.to_lowercase() != "false",
            Value::Nil => false,
            _ => true,
        }
    };

    let mut fields = HashMap::new();

    fields.insert("value".to_string(), Value::Bool(bool_value));

    fields.insert(
        "toString".to_string(),
        Value::Function("Boolean.toString".to_string()),
    );

    fields.insert(
        "TRUE".to_string(),
        Value::Function("Boolean.TRUE".to_string()),
    );
    fields.insert(
        "FALSE".to_string(),
        Value::Function("Boolean.FALSE".to_string()),
    );

    let object = Object {
        typ: ObjectType::Instance("Boolean".to_string()),
        fields,
    };

    Ok(Value::Object(Rc::new(RefCell::new(object))))
}

pub fn boolean_from(args: &[Value]) -> Result<Value, VanuaError> {
    boolean_new(args)
}

pub fn boolean_to_string(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Boolean.toString: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Boolean" => {
                    if let Some(Value::Bool(b)) = obj.fields.get("value") {
                        crate::stdlib::lang::string::string_new(&[Value::String(b.to_string())])
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Boolean.toString: boolean value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Boolean.toString: expected Boolean object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Boolean.toString: expected Boolean object".to_string(),
            cause: None,
        }),
    }
}

pub fn boolean_value_of(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Boolean.valueOf: expected 1 argument".to_string(),
            cause: None,
        });
    }

    boolean_new(args)
}

pub fn boolean_true(_args: &[Value]) -> Result<Value, VanuaError> {
    boolean_new(&[Value::Bool(true)])
}

pub fn boolean_false(_args: &[Value]) -> Result<Value, VanuaError> {
    boolean_new(&[Value::Bool(false)])
}

pub fn is_not_null(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "is_not_null: expected 1 argument".to_string(),
            cause: None,
        });
    }

    let is_not_null = !matches!(args[0], Value::Nil);
    Ok(Value::Bool(is_not_null))
}

pub fn bool_to_string(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "bool_to_string: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Bool(b) => Ok(Value::String(b.to_string())),
        _ => Err(VanuaError::RuntimeError {
            message: "bool_to_string: expected Bool value".to_string(),
            cause: None,
        }),
    }
}
