pub mod array;
pub mod boolean;
pub mod float;
pub mod integer;
pub mod map;
pub mod string;
pub mod unit;

use crate::error::VanuaError;
use crate::stdlib::StdlibModule;
use crate::vm::Value;

pub fn get_module() -> StdlibModule {
    let mut functions = Vec::new();

    functions.extend(string::get_functions());
    functions.extend(integer::get_functions());
    functions.extend(boolean::get_functions());
    functions.extend(float::get_functions());
    functions.extend(array::get_functions());
    functions.extend(map::get_functions());
    functions.extend(unit::get_functions());

    functions.push("auto_to_string".to_string());
    functions.push("string_concat".to_string());

    StdlibModule {
        name: "lang".to_string(),
        functions,
        constants: std::collections::HashMap::new(),
    }
}

pub fn auto_to_string(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "auto_to_string: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::String(s) => Ok(Value::String(s.clone())),
        Value::Int(i) => Ok(Value::String(i.to_string())),
        Value::Float(f) => Ok(Value::String(f.to_string())),
        Value::Bool(b) => Ok(Value::String(b.to_string())),
        Value::Char(c) => Ok(Value::String(c.to_string())),
        Value::Nil => Ok(Value::String("null".to_string())),
        Value::Unit => Ok(Value::String("()".to_string())),
        _ => Ok(Value::String(format!("{}", args[0]))),
    }
}

/// Concatenate two values as strings
pub fn string_concat(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "string_concat: expected 2 arguments".to_string(),
            cause: None,
        });
    }

    let left_str = auto_to_string(&[args[0].clone()])?;
    let right_str = auto_to_string(&[args[1].clone()])?;

    match (left_str, right_str) {
        (Value::String(left), Value::String(right)) => {
            Ok(Value::String(format!("{}{}", left, right)))
        }
        _ => Err(VanuaError::RuntimeError {
            message: "string_concat: failed to convert arguments to strings".to_string(),
            cause: None,
        }),
    }
}
