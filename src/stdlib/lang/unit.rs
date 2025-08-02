use crate::error::VanuaError;
use crate::vm::Value;

pub fn unit_to_string(args: &[Value]) -> Result<Value, VanuaError> {
    if args.is_empty() {
        return Err(VanuaError::RuntimeError {
            message: "toString() requires 'this' argument".to_string(),
            cause: None,
        });
    }

    let this = &args[0];
    let string_repr = match this {
        Value::Object(obj) => {
            let obj_ref = obj.borrow();
            match &obj_ref.typ {
                crate::vm::ObjectType::Instance(name) => format!("<instance {}>", name),
                crate::vm::ObjectType::Class(name) => format!("<class {}>", name),
                crate::vm::ObjectType::Closure(name) => format!("<function {}>", name),
                crate::vm::ObjectType::Unit => "<unit>".to_string(),
                _ => format!("<{}>", this.type_name()),
            }
        }
        Value::Array(arr) => {
            let elements: Result<Vec<String>, VanuaError> = arr
                .iter()
                .map(|v| match unit_to_string(&[v.clone()]) {
                    Ok(Value::String(s)) => Ok(s),
                    Ok(_) => Ok(format!("{}", v)),
                    Err(_) => Ok(format!("{}", v)),
                })
                .collect();

            match elements {
                Ok(strs) => format!("[{}]", strs.join(", ")),
                Err(_) => format!("{}", this),
            }
        }
        Value::Map(map) => {
            let mut entries = Vec::new();
            for (key, value) in map.iter() {
                let value_str = match unit_to_string(&[value.clone()]) {
                    Ok(Value::String(s)) => s,
                    Ok(_) => format!("{}", value),
                    Err(_) => format!("{}", value),
                };
                entries.push(format!("{}: {}", key, value_str));
            }
            format!("{{{}}}", entries.join(", "))
        }
        Value::Function(name) => format!("<function {}>", name),
        Value::Unit => "<unit>".to_string(),
        _ => format!("{}", this),
    };

    Ok(Value::String(string_repr))
}

pub fn unit_clone(args: &[Value]) -> Result<Value, VanuaError> {
    if args.is_empty() {
        return Err(VanuaError::RuntimeError {
            message: "clone() requires 'this' argument".to_string(),
            cause: None,
        });
    }

    let this = &args[0];
    match this {
        Value::Object(obj) => {
            let obj_ref = obj.borrow();
            let cloned_obj = crate::vm::Object {
                typ: obj_ref.typ.clone(),
                fields: obj_ref.fields.clone(),
            };
            Ok(Value::Object(std::rc::Rc::new(std::cell::RefCell::new(
                cloned_obj,
            ))))
        }
        Value::Array(arr) => Ok(Value::Array(arr.clone())),
        Value::Map(map) => Ok(Value::Map(map.clone())),

        _ => Ok(this.clone()),
    }
}

pub fn get_functions() -> Vec<String> {
    vec!["Unit.toString".to_string(), "Unit.clone".to_string()]
}
