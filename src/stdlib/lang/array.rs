use crate::error::VanuaError;
use crate::vm::{Object, ObjectType, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn get_functions() -> Vec<String> {
    vec![
        "Array.new".to_string(),
        "Array.from".to_string(),
        "Array.length".to_string(),
        "Array.get".to_string(),
        "Array.set".to_string(),
        "Array.push".to_string(),
        "Array.pop".to_string(),
        "Array.forEach".to_string(),
        "Array.map".to_string(),
        "Array.filter".to_string(),
        "Array.indexOf".to_string(),
        "Array.contains".to_string(),
        "Array.join".to_string(),
        "Array.slice".to_string(),
        "Array.iterator".to_string(),
    ]
}

/// Creates a new Array object with all methods
pub fn array_new(args: &[Value]) -> Result<Value, VanuaError> {
    let array_value = args.to_vec();

    let mut fields = HashMap::new();

    fields.insert("value".to_string(), Value::Array(array_value.clone()));

    fields.insert(
        "length".to_string(),
        Value::Function("Array.length".to_string()),
    );
    fields.insert("get".to_string(), Value::Function("Array.get".to_string()));
    fields.insert("set".to_string(), Value::Function("Array.set".to_string()));
    fields.insert(
        "push".to_string(),
        Value::Function("Array.push".to_string()),
    );
    fields.insert("pop".to_string(), Value::Function("Array.pop".to_string()));
    fields.insert(
        "forEach".to_string(),
        Value::Function("Array.forEach".to_string()),
    );
    fields.insert("map".to_string(), Value::Function("Array.map".to_string()));
    fields.insert(
        "filter".to_string(),
        Value::Function("Array.filter".to_string()),
    );
    fields.insert(
        "indexOf".to_string(),
        Value::Function("Array.indexOf".to_string()),
    );
    fields.insert(
        "contains".to_string(),
        Value::Function("Array.contains".to_string()),
    );
    fields.insert(
        "join".to_string(),
        Value::Function("Array.join".to_string()),
    );
    fields.insert(
        "slice".to_string(),
        Value::Function("Array.slice".to_string()),
    );
    fields.insert(
        "iterator".to_string(),
        Value::Function("Array.iterator".to_string()),
    );

    let object = Object {
        typ: ObjectType::Instance("Array".to_string()),
        fields,
    };

    Ok(Value::Object(Rc::new(RefCell::new(object))))
}

pub fn array_from(args: &[Value]) -> Result<Value, VanuaError> {
    array_new(args)
}

pub fn array_new_from_vec(values: &[Value]) -> Result<Value, VanuaError> {
    let mut fields = HashMap::new();

    fields.insert("value".to_string(), Value::Array(values.to_vec()));

    fields.insert(
        "length".to_string(),
        Value::Function("Array.length".to_string()),
    );
    fields.insert("get".to_string(), Value::Function("Array.get".to_string()));
    fields.insert("set".to_string(), Value::Function("Array.set".to_string()));
    fields.insert(
        "push".to_string(),
        Value::Function("Array.push".to_string()),
    );
    fields.insert("pop".to_string(), Value::Function("Array.pop".to_string()));
    fields.insert(
        "forEach".to_string(),
        Value::Function("Array.forEach".to_string()),
    );
    fields.insert("map".to_string(), Value::Function("Array.map".to_string()));
    fields.insert(
        "filter".to_string(),
        Value::Function("Array.filter".to_string()),
    );
    fields.insert(
        "indexOf".to_string(),
        Value::Function("Array.indexOf".to_string()),
    );
    fields.insert(
        "contains".to_string(),
        Value::Function("Array.contains".to_string()),
    );
    fields.insert(
        "join".to_string(),
        Value::Function("Array.join".to_string()),
    );
    fields.insert(
        "slice".to_string(),
        Value::Function("Array.slice".to_string()),
    );

    let object = Object {
        typ: ObjectType::Instance("Array".to_string()),
        fields,
    };

    Ok(Value::Object(Rc::new(RefCell::new(object))))
}

pub fn array_length(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Array.length: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        Ok(Value::Int(arr.len() as i64))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.length: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.length: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.length: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_get(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Array.get: expected 2 arguments (this, index)".to_string(),
            cause: None,
        });
    }

    let index = match &args[1] {
        Value::Int(i) => *i as usize,
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Array.get: index must be an integer".to_string(),
                cause: None,
            })
        }
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        if index < arr.len() {
                            Ok(arr[index].clone())
                        } else {
                            Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Array.get: index {} out of bounds for array of length {}",
                                    index,
                                    arr.len()
                                ),
                                cause: None,
                            })
                        }
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.get: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.get: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.get: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_set(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 3 {
        return Err(VanuaError::RuntimeError {
            message: "Array.set: expected 3 arguments (this, index, value)".to_string(),
            cause: None,
        });
    }

    let index = match &args[1] {
        Value::Int(i) => *i as usize,
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Array.set: index must be an integer".to_string(),
                cause: None,
            })
        }
    };

    let value = args[2].clone();

    match &args[0] {
        Value::Object(obj) => {
            let mut obj_ref = obj.borrow_mut();
            match obj_ref.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(ref mut arr)) = obj_ref.fields.get_mut("value") {
                        if index < arr.len() {
                            arr[index] = value.clone();
                            Ok(value)
                        } else {
                            Err(VanuaError::RuntimeError {
                                message: format!(
                                    "Array.set: index {} out of bounds for array of length {}",
                                    index,
                                    arr.len()
                                ),
                                cause: None,
                            })
                        }
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.set: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.set: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.set: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_push(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Array.push: expected 2 arguments (this, value)".to_string(),
            cause: None,
        });
    }

    let value = args[1].clone();

    match &args[0] {
        Value::Object(obj) => {
            let mut obj_ref = obj.borrow_mut();
            match obj_ref.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(ref mut arr)) = obj_ref.fields.get_mut("value") {
                        arr.push(value);
                        Ok(Value::Int(arr.len() as i64))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.push: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.push: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.push: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_pop(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Array.pop: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let mut obj_ref = obj.borrow_mut();
            match obj_ref.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(ref mut arr)) = obj_ref.fields.get_mut("value") {
                        if arr.is_empty() {
                            Ok(Value::Nil)
                        } else {
                            Ok(arr.pop().unwrap())
                        }
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.pop: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.pop: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.pop: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_index_of(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Array.indexOf: expected 2 arguments (this, element)".to_string(),
            cause: None,
        });
    }

    let element = &args[1];

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        for (i, item) in arr.iter().enumerate() {
                            if item == element {
                                return Ok(Value::Int(i as i64));
                            }
                        }
                        Ok(Value::Int(-1))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.indexOf: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.indexOf: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.indexOf: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_contains(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Array.contains: expected 2 arguments (this, element)".to_string(),
            cause: None,
        });
    }

    let element = &args[1];

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        Ok(Value::Bool(arr.contains(element)))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.contains: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.contains: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.contains: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_join(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() < 1 || args.len() > 2 {
        return Err(VanuaError::RuntimeError {
            message: "Array.join: expected 1 or 2 arguments (this, [separator])".to_string(),
            cause: None,
        });
    }

    let separator = if args.len() == 2 {
        match &args[1] {
            Value::String(s) => s.clone(),
            _ => {
                return Err(VanuaError::RuntimeError {
                    message: "Array.join: separator must be a string".to_string(),
                    cause: None,
                })
            }
        }
    } else {
        ",".to_string()
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        let string_elements: Vec<String> =
                            arr.iter().map(|v| format!("{}", v)).collect();

                        let joined = string_elements.join(&separator);

                        crate::stdlib::lang::string::string_new(&[Value::String(joined)])
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.join: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.join: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.join: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_slice(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 3 {
        return Err(VanuaError::RuntimeError {
            message: "Array.slice: expected 3 arguments (this, start, end)".to_string(),
            cause: None,
        });
    }

    let start = match &args[1] {
        Value::Int(i) => *i as usize,
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Array.slice: start must be an integer".to_string(),
                cause: None,
            })
        }
    };

    let end = match &args[2] {
        Value::Int(i) => *i as usize,
        _ => {
            return Err(VanuaError::RuntimeError {
                message: "Array.slice: end must be an integer".to_string(),
                cause: None,
            })
        }
    };

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        if start > end || end > arr.len() {
                            return Err(VanuaError::RuntimeError {
                                message: format!("Array.slice: invalid range (start: {}, end: {}) for array of length {}",
                                                start, end, arr.len()),
                                cause: None,
                            });
                        }
                        let slice: Vec<Value> = arr[start..end].to_vec();

                        array_new(&slice)
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.slice: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.slice: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.slice: expected Array object".to_string(),
            cause: None,
        }),
    }
}

/// Esegue una funzione per ogni elemento dell'array
pub fn array_for_each(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Array.forEach: expected 2 arguments (this, callback)".to_string(),
            cause: None,
        });
    }

    let callback = &args[1];

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        for (index, element) in arr.iter().enumerate() {
                            match callback {
                                Value::Function(func_name) => {
                                    let args = vec![element.clone(), Value::Int(index as i64)];
                                    if let Err(e) =
                                        crate::stdlib::call_stdlib_function(&func_name, &args)
                                    {
                                        return Err(VanuaError::RuntimeError {
                                            message: format!(
                                                "Array.forEach: callback error: {}",
                                                e
                                            ),
                                            cause: Some(Box::new(e)),
                                        });
                                    }
                                }
                                _ => {
                                    return Err(VanuaError::RuntimeError {
                                        message: "Array.forEach: callback must be a function"
                                            .to_string(),
                                        cause: None,
                                    });
                                }
                            }
                        }
                        Ok(Value::Nil)
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.forEach: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.forEach: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.forEach: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_map(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Array.map: expected 2 arguments (this, callback)".to_string(),
            cause: None,
        });
    }

    let callback = &args[1];

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        let mut mapped_array = Vec::new();

                        for (index, element) in arr.iter().enumerate() {
                            match callback {
                                Value::Function(func_name) => {
                                    let args = vec![element.clone(), Value::Int(index as i64)];
                                    match crate::stdlib::call_stdlib_function(&func_name, &args) {
                                        Ok(result) => mapped_array.push(result),
                                        Err(e) => {
                                            return Err(VanuaError::RuntimeError {
                                                message: format!(
                                                    "Array.map: callback error: {}",
                                                    e
                                                ),
                                                cause: Some(Box::new(e)),
                                            });
                                        }
                                    }
                                }
                                _ => {
                                    return Err(VanuaError::RuntimeError {
                                        message: "Array.map: callback must be a function"
                                            .to_string(),
                                        cause: None,
                                    });
                                }
                            }
                        }

                        array_new_from_vec(&mapped_array)
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.map: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.map: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.map: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_filter(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "Array.filter: expected 2 arguments (this, predicate)".to_string(),
            cause: None,
        });
    }

    let predicate = &args[1];

    match &args[0] {
        // FIXME
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        let mut filtered_array = Vec::new();

                        for (index, element) in arr.iter().enumerate() {
                            match predicate {
                                Value::Function(func_name) => {
                                    let args = vec![element.clone(), Value::Int(index as i64)];
                                    match crate::stdlib::call_stdlib_function(&func_name, &args) {
                                        Ok(result) => {
                                            if result.is_truthy() {
                                                filtered_array.push(element.clone());
                                            }
                                        }
                                        Err(e) => {
                                            return Err(VanuaError::RuntimeError {
                                                message: format!(
                                                    "Array.filter: predicate error: {}",
                                                    e
                                                ),
                                                cause: Some(Box::new(e)),
                                            });
                                        }
                                    }
                                }
                                _ => {
                                    return Err(VanuaError::RuntimeError {
                                        message: "Array.filter: predicate must be a function"
                                            .to_string(),
                                        cause: None,
                                    });
                                }
                            }
                        }

                        array_new_from_vec(&filtered_array)
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.filter: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.filter: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.filter: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_iterator(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "Array.iterator: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let obj = obj.borrow();
            match obj.typ {
                ObjectType::Instance(ref name) if name == "Array" => {
                    if let Some(Value::Array(arr)) = obj.fields.get("value") {
                        let mut iterator_fields = HashMap::new();

                        iterator_fields.insert("array".to_string(), Value::Array(arr.clone()));
                        iterator_fields.insert("index".to_string(), Value::Int(0));

                        iterator_fields.insert(
                            "next".to_string(),
                            Value::Function("ArrayIterator.next".to_string()),
                        );

                        let iterator_obj = Object {
                            typ: ObjectType::Instance("ArrayIterator".to_string()),
                            fields: iterator_fields,
                        };

                        Ok(Value::Object(Rc::new(RefCell::new(iterator_obj))))
                    } else {
                        Err(VanuaError::RuntimeError {
                            message: "Array.iterator: array value not found".to_string(),
                            cause: None,
                        })
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "Array.iterator: expected Array object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "Array.iterator: expected Array object".to_string(),
            cause: None,
        }),
    }
}

pub fn array_iterator_next(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "ArrayIterator.next: expected 1 argument (this)".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Object(obj) => {
            let mut obj_ref = obj.borrow_mut();
            match obj_ref.typ {
                ObjectType::Instance(ref name) if name == "ArrayIterator" => {
                    let current_index = if let Some(Value::Int(idx)) = obj_ref.fields.get("index") {
                        *idx as usize
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "ArrayIterator.next: index not found".to_string(),
                            cause: None,
                        });
                    };

                    let array = if let Some(Value::Array(arr)) = obj_ref.fields.get("array") {
                        arr.clone()
                    } else {
                        return Err(VanuaError::RuntimeError {
                            message: "ArrayIterator.next: array not found".to_string(),
                            cause: None,
                        });
                    };

                    if current_index < array.len() {
                        obj_ref
                            .fields
                            .insert("index".to_string(), Value::Int((current_index + 1) as i64));

                        Ok(array[current_index].clone())
                    } else {
                        Ok(Value::Nil)
                    }
                }
                _ => Err(VanuaError::RuntimeError {
                    message: "ArrayIterator.next: expected ArrayIterator object".to_string(),
                    cause: None,
                }),
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "ArrayIterator.next: expected ArrayIterator object".to_string(),
            cause: None,
        }),
    }
}
