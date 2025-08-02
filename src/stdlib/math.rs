use crate::error::VanuaError;
use crate::vm::Value;
use rand::Rng;

/// Square root function
pub fn sqrt(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "sqrt: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => {
            if *i < 0 {
                return Err(VanuaError::RuntimeError {
                    message: "sqrt: cannot take square root of negative number".to_string(),
                    cause: None,
                });
            }
            Ok(Value::Float((*i as f64).sqrt()))
        }
        Value::Float(f) => {
            if *f < 0.0 {
                return Err(VanuaError::RuntimeError {
                    message: "sqrt: cannot take square root of negative number".to_string(),
                    cause: None,
                });
            }
            Ok(Value::Float(f.sqrt()))
        }
        _ => Err(VanuaError::RuntimeError {
            message: "sqrt: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

/// Power function
pub fn pow(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "pow: expected 2 arguments".to_string(),
            cause: None,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Int(base), Value::Int(exp)) => {
            if *exp >= 0 {
                let result = (*base as f64).powi(*exp as i32);
                if result.fract() == 0.0 && result.abs() <= (i64::MAX as f64) {
                    Ok(Value::Int(result as i64))
                } else {
                    Ok(Value::Float(result))
                }
            } else {
                Ok(Value::Float((*base as f64).powi(*exp as i32)))
            }
        }
        (Value::Int(base), Value::Float(exp)) => Ok(Value::Float((*base as f64).powf(*exp))),
        (Value::Float(base), Value::Int(exp)) => Ok(Value::Float(base.powi(*exp as i32))),
        (Value::Float(base), Value::Float(exp)) => Ok(Value::Float(base.powf(*exp))),
        _ => Err(VanuaError::RuntimeError {
            message: "pow: expected numeric arguments".to_string(),
            cause: None,
        }),
    }
}

/// Absolute value function
pub fn abs(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "abs: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => Ok(Value::Int(i.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        _ => Err(VanuaError::RuntimeError {
            message: "abs: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

/// Round to nearest integer
pub fn round(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "round: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => Ok(Value::Int(*i)),
        Value::Float(f) => {
            let rounded = f.round();
            if rounded.abs() <= (i64::MAX as f64) {
                Ok(Value::Int(rounded as i64))
            } else {
                Ok(Value::Float(rounded))
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "round: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

/// Round down to nearest integer
pub fn floor(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "floor: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => Ok(Value::Int(*i)),
        Value::Float(f) => {
            let floored = f.floor();
            if floored.abs() <= (i64::MAX as f64) {
                Ok(Value::Int(floored as i64))
            } else {
                Ok(Value::Float(floored))
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "floor: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

/// Round up to nearest integer
pub fn ceil(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "ceil: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => Ok(Value::Int(*i)),
        Value::Float(f) => {
            let ceiled = f.ceil();
            if ceiled.abs() <= (i64::MAX as f64) {
                Ok(Value::Int(ceiled as i64))
            } else {
                Ok(Value::Float(ceiled))
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "ceil: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

/// Minimum of two values
pub fn min(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "min: expected 2 arguments".to_string(),
            cause: None,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
        (Value::Float(a), Value::Float(b)) => {
            if a <= b {
                Ok(Value::Float(*a))
            } else {
                Ok(Value::Float(*b))
            }
        }
        (Value::Int(a), Value::Float(b)) => {
            let a_float = *a as f64;
            if a_float <= *b {
                Ok(Value::Int(*a))
            } else {
                Ok(Value::Float(*b))
            }
        }
        (Value::Float(a), Value::Int(b)) => {
            let b_float = *b as f64;
            if *a <= b_float {
                Ok(Value::Float(*a))
            } else {
                Ok(Value::Int(*b))
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "min: expected numeric arguments".to_string(),
            cause: None,
        }),
    }
}

/// Maximum of two values
pub fn max(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 2 {
        return Err(VanuaError::RuntimeError {
            message: "max: expected 2 arguments".to_string(),
            cause: None,
        });
    }

    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
        (Value::Float(a), Value::Float(b)) => {
            if a >= b {
                Ok(Value::Float(*a))
            } else {
                Ok(Value::Float(*b))
            }
        }
        (Value::Int(a), Value::Float(b)) => {
            let a_float = *a as f64;
            if a_float >= *b {
                Ok(Value::Int(*a))
            } else {
                Ok(Value::Float(*b))
            }
        }
        (Value::Float(a), Value::Int(b)) => {
            let b_float = *b as f64;
            if *a >= b_float {
                Ok(Value::Float(*a))
            } else {
                Ok(Value::Int(*b))
            }
        }
        _ => Err(VanuaError::RuntimeError {
            message: "max: expected numeric arguments".to_string(),
            cause: None,
        }),
    }
}

/// Generate a random number
pub fn random(args: &[Value]) -> Result<Value, VanuaError> {
    match args.len() {
        0 => {
            let value = rand::thread_rng().gen::<f64>();
            Ok(Value::Float(value))
        }
        1 => match &args[0] {
            Value::Int(n) => {
                if *n <= 0 {
                    return Err(VanuaError::RuntimeError {
                        message: "random: upper bound must be positive".to_string(),
                        cause: None,
                    });
                }
                let value = rand::thread_rng().gen_range(0..*n);
                Ok(Value::Int(value))
            }
            _ => Err(VanuaError::RuntimeError {
                message: "random: expected integer argument".to_string(),
                cause: None,
            }),
        },
        2 => match (&args[0], &args[1]) {
            (Value::Int(min), Value::Int(max)) => {
                if min >= max {
                    return Err(VanuaError::RuntimeError {
                        message: "random: min must be less than max".to_string(),
                        cause: None,
                    });
                }
                let value = rand::thread_rng().gen_range(*min..*max);
                Ok(Value::Int(value))
            }
            _ => Err(VanuaError::RuntimeError {
                message: "random: expected integer arguments".to_string(),
                cause: None,
            }),
        },
        _ => Err(VanuaError::RuntimeError {
            message: "random: expected 0, 1, or 2 arguments".to_string(),
            cause: None,
        }),
    }
}

/// Sine function
pub fn sin(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "sin: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => Ok(Value::Float((*i as f64).sin())),
        Value::Float(f) => Ok(Value::Float(f.sin())),
        _ => Err(VanuaError::RuntimeError {
            message: "sin: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

/// Cosine function
pub fn cos(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "cos: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => Ok(Value::Float((*i as f64).cos())),
        Value::Float(f) => Ok(Value::Float(f.cos())),
        _ => Err(VanuaError::RuntimeError {
            message: "cos: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

/// Tangent function
pub fn tan(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "tan: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => Ok(Value::Float((*i as f64).tan())),
        Value::Float(f) => Ok(Value::Float(f.tan())),
        _ => Err(VanuaError::RuntimeError {
            message: "tan: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

/// Natural logarithm function
pub fn log(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "log: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => {
            if *i <= 0 {
                return Err(VanuaError::RuntimeError {
                    message: "log: cannot take logarithm of non-positive number".to_string(),
                    cause: None,
                });
            }
            Ok(Value::Float((*i as f64).ln()))
        }
        Value::Float(f) => {
            if *f <= 0.0 {
                return Err(VanuaError::RuntimeError {
                    message: "log: cannot take logarithm of non-positive number".to_string(),
                    cause: None,
                });
            }
            Ok(Value::Float(f.ln()))
        }
        _ => Err(VanuaError::RuntimeError {
            message: "log: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

/// Exponential function
pub fn exp(args: &[Value]) -> Result<Value, VanuaError> {
    if args.len() != 1 {
        return Err(VanuaError::RuntimeError {
            message: "exp: expected 1 argument".to_string(),
            cause: None,
        });
    }

    match &args[0] {
        Value::Int(i) => Ok(Value::Float((*i as f64).exp())),
        Value::Float(f) => Ok(Value::Float(f.exp())),
        _ => Err(VanuaError::RuntimeError {
            message: "exp: expected numeric argument".to_string(),
            cause: None,
        }),
    }
}

pub fn get_module() -> super::StdlibModule {
    use std::collections::HashMap;

    let mut constants = HashMap::new();
    constants.insert(
        "PI".to_string(),
        crate::vm::Value::Float(std::f64::consts::PI),
    );
    constants.insert(
        "E".to_string(),
        crate::vm::Value::Float(std::f64::consts::E),
    );

    super::StdlibModule {
        name: "math".to_string(),
        functions: vec![
            "sqrt".to_string(),
            "pow".to_string(),
            "abs".to_string(),
            "round".to_string(),
            "floor".to_string(),
            "ceil".to_string(),
            "min".to_string(),
            "max".to_string(),
            "random".to_string(),
            "sin".to_string(),
            "cos".to_string(),
            "tan".to_string(),
            "log".to_string(),
            "exp".to_string(),
        ],
        constants,
    }
}
