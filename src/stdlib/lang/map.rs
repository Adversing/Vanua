use crate::error::VanuaError;
use crate::vm::{Object, ObjectType, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn get_functions() -> Vec<String> {
    vec![
        "Map.new".to_string(),
        "Map.from".to_string(),
        "Map.get".to_string(),
        "Map.set".to_string(),
        "Map.has".to_string(),
        "Map.remove".to_string(),
        "Map.keys".to_string(),
        "Map.values".to_string(),
        "Map.size".to_string(),
        "Map.clear".to_string(),
        "Map.toString".to_string(),
    ]
}

fn map_new_from_hashmap(map_value: &HashMap<String, Value>) -> Result<Value, VanuaError> {
    let mut fields = HashMap::new();

    fields.insert("value".to_string(), Value::Map(map_value.clone()));

    fields.insert("get".to_string(), Value::Function("Map.get".to_string()));
    fields.insert("set".to_string(), Value::Function("Map.set".to_string()));
    fields.insert("has".to_string(), Value::Function("Map.has".to_string()));
    fields.insert(
        "remove".to_string(),
        Value::Function("Map.remove".to_string()),
    );
    fields.insert("keys".to_string(), Value::Function("Map.keys".to_string()));
    fields.insert(
        "values".to_string(),
        Value::Function("Map.values".to_string()),
    );
    fields.insert("size".to_string(), Value::Function("Map.size".to_string()));
    fields.insert(
        "clear".to_string(),
        Value::Function("Map.clear".to_string()),
    );
    fields.insert(
        "toString".to_string(),
        Value::Function("Map.toString".to_string()),
    );

    let object = Object {
        typ: ObjectType::Instance("Map".to_string()),
        fields,
    };

    Ok(Value::Object(Rc::new(RefCell::new(object))))
}

pub fn map_new(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() % 2 != 0 {
        return Err(VanuaError::RuntimeError {
            message: "Map.new: expected even number of arguments (key, value pairs)".to_string(),
            cause: None,
        });
    }

    let mut map_value = HashMap::new();

    for i in (0..args.len()).step_by(2) {
        let key = match &args[i] {
            Value::String(s) => s.clone(),
            v => format!("{}", v),
        };
        map_value.insert(key, args[i + 1].clone());
    }

    map_new_from_hashmap(&map_value)
}

pub fn map_from(args: &[Value]) -> Result<Value, VanuaError> {
    map_new(args)
}

pub fn map_get(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Map.get: expected 2 arguments (this, key)".to_string(),
            cause: None,
        });
    }

    let key = match &args[1] {
        Value::String(s) => s.clone(),
        v => format!("{}", v),
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Map" => {
                    if let Some(Value::Map(map)) = obj.fields.get("value") {
                        Ok(map.get(&key).cloned().unwrap_or(Value::Nil))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Map.get: map value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Map.get: expected Map object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Map.get: expected Map object".to_string(),
            cause: None,
        }),
    }
}

pub fn map_set(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 3 {
        return Err(VanuaError::RuntimeError {
            message: "Map.set: expected 3 arguments (this, key, value)".to_string(),
            cause: None,
        });
    }

    let key = match &args[1] {
        Value::String(s) => s.clone(),
        v => format!("{}", v),
    };

    let value = args[2].clone();

    match &args[0] {
        Value::Object(obj) => {
            let mut obj_ref = obj.borrow_mut();
            match obj_ref.typ {
                ObjectType::Instance(ref name) if name == "Map" => {
                    if let Some(Value::Map(ref mut map)) = obj_ref.fields.get_mut("value") {
                        map.insert(key, value.clone());
                        Ok(value)
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Map.set: map value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Map.set: expected Map object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Map.set: expected Map object".to_string(),
            cause: None,
        }),
    }
}

pub fn map_has(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Map.has: expected 2 arguments (this, key)".to_string(),
            cause: None,
        });
    }

    let key = match &args[1] {
        Value::String(s) => s.clone(),
        v => format!("{}", v),
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Map" => {
                    if let Some(Value::Map(map)) = obj.fields.get("value") {
                        Ok(Value::Bool(map.contains_key(&key)))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Map.has: map value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Map.has: expected Map object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Map.has: expected Map object".to_string(),
            cause: None,
        }),
    }
}

pub fn map_remove(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Map.remove: expected 2 arguments (this, key)".to_string(),
            cause: None,
        });
    }

    let key = match &args[1] {
        Value::String(s) => s.clone(),
        v => format!("{}", v),
    };

    match &args[0] {
        Value::Object(obj) => {
            let mut obj_ref = obj.borrow_mut();
            match obj_ref.typ {
                ObjectType::Instance(ref name) if name == "Map" => {
                    if let Some(Value::Map(ref mut map)) = obj_ref.fields.get_mut("value") {
                        let removed = map.remove(&key).unwrap_or(Value::Nil);
                        Ok(removed)
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Map.remove: map value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Map.remove: expected Map object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Map.remove: expected Map object".to_string(),
            cause: None,
        }),
    }
}

pub fn map_keys(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Map.keys: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Map" => {
                    if let Some(Value::Map(map)) = obj.fields.get("value") {
                        let keys: Vec<Value> =
                            map.keys().map(|k| Value::String(k.clone())).collect();

                        crate::stdlib::lang::array::array_new(&keys)
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Map.keys: map value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Map.keys: expected Map object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Map.keys: expected Map object".to_string(),
            cause: None,
        }),
    }
}

pub fn map_values(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Map.values: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Map" => {
                    if let Some(Value::Map(map)) = obj.fields.get("value") {
                        let values: Vec<Value> = map.values().cloned().collect();

                        crate::stdlib::lang::array::array_new(&values)
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Map.values: map value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Map.values: expected Map object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Map.values: expected Map object".to_string(),
            cause: None,
        }),
    }
}

pub fn map_size(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Map.size: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Map" => {
                    if let Some(Value::Map(map)) = obj.fields.get("value") {
                        Ok(Value::Int(map.len() as i64))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Map.size: map value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Map.size: expected Map object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Map.size: expected Map object".to_string(),
            cause: None,
        }),
    }
}

pub fn map_clear(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Map.clear: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let mut obj_ref = obj.borrow_mut();
            match obj_ref.typ {
                ObjectType::Instance(ref name) if name == "Map" => {
                    if let Some(Value::Map(ref mut map)) = obj_ref.fields.get_mut("value") {
                        map.clear();
                        Ok(Value::Nil)
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Map.clear: map value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Map.clear: expected Map object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Map.clear: expected Map object".to_string(),
            cause: None,
        }),
    }
}

pub fn map_to_string(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Map.toString: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Map" => {
                    if let Some(Value::Map(map)) = obj.fields.get("value") {
                        let mut entries = Vec::new();
                        for (key, value) in map {
                            entries.push(format!("{}={}", key, value));
                        }
                        let result = format!("{{{}}}", entries.join(", "));

                        Ok(Value::String(result))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Map.toString: map value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Map.toString: expected Map object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Map.toString: expected Map object".to_string(),
            cause: None,
        }),
    }
}
