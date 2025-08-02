use rand;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::sync::mpsc::Receiver;
use std::sync::{Arc, Mutex};
use tokio::sync::oneshot;

/// Values that can be manipulated by the VM
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Object(Rc<RefCell<Object>>),
    Function(String),
    Array(Vec<Value>),
    Map(HashMap<String, Value>),
    Nil,
    Unit,

    Curry(Rc<RefCell<CurriedFunction>>),
    Compose(Rc<RefCell<ComposedFunction>>),
    Partial(Rc<RefCell<PartialFunction>>),
    Lazy(Rc<RefCell<LazyValue>>),
    Pattern(Rc<RefCell<MatchPattern>>),

    Future(Arc<Mutex<FutureValue>>),
}

/// Future value for asynchronous operations
#[derive(Debug)]
pub struct FutureValue {
    pub completed: bool,
    pub value: Option<SimpleFutureValue>,
    pub receiver: Option<Receiver<SimpleFutureValue>>,
    pub oneshot_receiver: Option<oneshot::Receiver<SimpleFutureValue>>,
}

/// Simplified value type that can be sent between threads
#[derive(Debug, Clone)]
pub enum SimpleFutureValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Nil,
}

/// Reference-counted object for heap-allocated values
#[derive(Debug, Clone)]
pub struct Object {
    pub typ: ObjectType,
    pub fields: HashMap<String, Value>,
}

/// Types of objects
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectType {
    Instance(String),
    Class(String),
    Struct(String),
    Closure(String),
    Unit,

    Curry,
    Compose,
    Partial,
    Lazy,
    Pattern,

    Future,
}

/// Curried function representation
#[derive(Debug, Clone)]
pub struct CurriedFunction {
    pub original: Value,
    pub applied_args: Vec<Value>,
    pub remaining_params: usize,
}

/// Composed function representation
#[derive(Debug, Clone)]
pub struct ComposedFunction {
    pub f: Value,
    pub g: Value,
}

/// Partially applied function
#[derive(Debug, Clone)]
pub struct PartialFunction {
    pub original: Value,
    pub args: Vec<Option<Value>>,
}

/// Lazy-evaluated value
#[derive(Debug, Clone)]
pub struct LazyValue {
    pub thunk: Value,
    pub evaluated: bool,
    pub value: Option<Value>,
}

/// Pattern for pattern matching
#[derive(Debug, Clone)]
pub struct MatchPattern {
    pub pattern_type: PatternType,
    pub guard: Option<Value>,
}

/// Types of patterns for pattern matching
#[derive(Debug, Clone)]
pub enum PatternType {
    Wildcard,
    Literal(Value),
    Variable(String),
    TypeTest(String),
    Array(Vec<Rc<RefCell<MatchPattern>>>),
    Object(HashMap<String, Rc<RefCell<MatchPattern>>>),
    Or(Rc<RefCell<MatchPattern>>, Rc<RefCell<MatchPattern>>),
    And(Rc<RefCell<MatchPattern>>, Rc<RefCell<MatchPattern>>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Nil, Value::Nil) => true,

            (Value::Object(a), Value::Object(b)) => Rc::ptr_eq(a, b),
            (Value::Curry(a), Value::Curry(b)) => Rc::ptr_eq(a, b),
            (Value::Compose(a), Value::Compose(b)) => Rc::ptr_eq(a, b),
            (Value::Partial(a), Value::Partial(b)) => Rc::ptr_eq(a, b),
            (Value::Lazy(a), Value::Lazy(b)) => Rc::ptr_eq(a, b),
            (Value::Pattern(a), Value::Pattern(b)) => Rc::ptr_eq(a, b),
            (Value::Future(a), Value::Future(b)) => Arc::ptr_eq(a, b),

            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,

            _ => false,
        }
    }
}

impl PartialEq for PatternType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PatternType::Wildcard, PatternType::Wildcard) => true,
            (PatternType::Literal(a), PatternType::Literal(b)) => a == b,
            (PatternType::Variable(a), PatternType::Variable(b)) => a == b,
            (PatternType::TypeTest(a), PatternType::TypeTest(b)) => a == b,

            (PatternType::Array(a), PatternType::Array(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter().zip(b.iter()).all(|(a, b)| Rc::ptr_eq(a, b))
            }
            (PatternType::Object(a), PatternType::Object(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter().all(|(key, val)| {
                    if let Some(other_val) = b.get(key) {
                        Rc::ptr_eq(val, other_val)
                    } else {
                        false
                    }
                })
            }
            (PatternType::Or(a1, a2), PatternType::Or(b1, b2)) => {
                Rc::ptr_eq(a1, b1) && Rc::ptr_eq(a2, b2)
            }
            (PatternType::And(a1, a2), PatternType::And(b1, b2)) => {
                Rc::ptr_eq(a1, b1) && Rc::ptr_eq(a2, b2)
            }

            _ => false,
        }
    }
}

impl Value {
    pub fn base_object() -> Self {
        let mut fields = HashMap::new();

        fields.insert(
            "toString".to_string(),
            Value::Function("Unit.toString".to_string()),
        );
        fields.insert(
            "clone".to_string(),
            Value::Function("Unit.clone".to_string()),
        );

        let object = Object {
            typ: ObjectType::Unit,
            fields,
        };

        Value::Object(Rc::new(RefCell::new(object)))
    }

    pub fn should_inherit_from_unit(&self) -> bool {
        match self {
            Value::Object(obj) => match &obj.borrow().typ {
                ObjectType::Instance(_) | ObjectType::Class(_) | ObjectType::Closure(_) => true,

                ObjectType::Unit => true,

                ObjectType::Struct(_)
                | ObjectType::Curry
                | ObjectType::Compose
                | ObjectType::Partial
                | ObjectType::Lazy
                | ObjectType::Pattern
                | ObjectType::Future => false,
            },

            Value::Array(_) | Value::Map(_) => true,

            Value::Function(_) => true,

            Value::Int(_)
            | Value::Float(_)
            | Value::Bool(_)
            | Value::Char(_)
            | Value::String(_)
            | Value::Nil => false,

            Value::Curry(_)
            | Value::Compose(_)
            | Value::Partial(_)
            | Value::Lazy(_)
            | Value::Pattern(_)
            | Value::Future(_) => false,

            Value::Unit => true,
        }
    }
    /// check if a value is truthy (used in conditional expressions)
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(a) => !a.is_empty(),
            Value::Map(m) => !m.is_empty(),
            Value::Future(f) => {
                let future = f.lock().unwrap();
                if future.completed {
                    if let Some(val) = &future.value {
                        match val {
                            SimpleFutureValue::Nil => false,
                            SimpleFutureValue::Bool(b) => *b,
                            SimpleFutureValue::Int(i) => *i != 0,
                            SimpleFutureValue::Float(f) => *f != 0.0,
                            SimpleFutureValue::String(s) => !s.is_empty(),
                            _ => true,
                        }
                    } else {
                        false
                    }
                } else {
                    true
                }
            }
            _ => true,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "Int",
            Value::Float(_) => "Float",
            Value::Bool(_) => "Bool",
            Value::Char(_) => "Char",
            Value::String(_) => "String",
            Value::Object(obj) => match &obj.borrow().typ {
                ObjectType::Instance(_) => "Instance",
                ObjectType::Class(_) => "Class",
                ObjectType::Struct(_) => "Struct",
                ObjectType::Closure(_) => "Function",
                ObjectType::Unit => "Unit",
                ObjectType::Curry => "CurriedFunction",
                ObjectType::Compose => "ComposedFunction",
                ObjectType::Partial => "PartialFunction",
                ObjectType::Lazy => "LazyValue",
                ObjectType::Pattern => "Pattern",
                ObjectType::Future => "Future",
            },
            Value::Function(_) => "Function",
            Value::Array(_) => "Array",
            Value::Map(_) => "Map",
            Value::Nil => "Nil",
            Value::Unit => "Unit",
            Value::Curry(_) => "CurriedFunction",
            Value::Compose(_) => "ComposedFunction",
            Value::Partial(_) => "PartialFunction",
            Value::Lazy(_) => "LazyValue",
            Value::Pattern(_) => "Pattern",
            Value::Future(_) => "Future",
        }
    }

    pub fn force(&self) -> Self {
        // TODO this must be completely reworked
        match self {
            Value::Lazy(lazy) => {
                let mut l = lazy.borrow_mut();
                if !l.evaluated {
                    // TODO: call the VM to execute the thunk
                    let thunk = l.thunk.clone();

                    let result = match thunk {
                        Value::Function(name) => match name.as_str() {
                            "identity" => Value::Int(0),
                            "pi" => Value::Float(std::f64::consts::PI),
                            "e" => Value::Float(std::f64::consts::E),
                            "random" => Value::Float(rand::random::<f64>()),
                            "time" => Value::Int(
                                std::time::SystemTime::now()
                                    .duration_since(std::time::UNIX_EPOCH)
                                    .unwrap_or_default()
                                    .as_secs() as i64,
                            ),
                            _ => crate::stdlib::call_stdlib_function(&name, &[])
                                .unwrap_or_else(|_| Value::Nil),
                        },
                        Value::Object(obj) => {
                            if let Some(Value::Function(method_name)) =
                                obj.borrow().fields.get("evaluate")
                            {
                                match crate::stdlib::call_stdlib_function(
                                    method_name,
                                    &[Value::Object(obj.clone())],
                                ) {
                                    Ok(result) => result,
                                    Err(_) => Value::Object(obj.clone()),
                                }
                            } else {
                                Value::Object(obj.clone())
                            }
                        }
                        _ => thunk.clone(),
                    };

                    l.value = Some(result.clone());
                    l.evaluated = true;

                    result
                } else {
                    l.value.clone().unwrap_or(Value::Nil)
                }
            }
            _ => self.clone(),
        }
    }

    /// Apply a function to arguments
    pub fn apply(&self, args: Vec<Value>) -> Result<Value, String> {
        // TODO this must be completely reworked too, it doesn't abide to SRP
        match self {
            Value::Function(name) => match name.as_str() {
                "add" => {
                    if args.len() != 2 {
                        return Err(format!("add: expected 2 arguments, got {}", args.len()));
                    }
                    match (&args[0], &args[1]) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
                        (Value::String(a), Value::String(b)) => {
                            let mut new_string = String::with_capacity(a.len() + b.len());
                            new_string.push_str(a);
                            new_string.push_str(b);
                            Ok(Value::String(new_string))
                        }
                        (Value::String(a), b) => {
                            let mut new_string = String::with_capacity(a.len() + 10);
                            new_string.push_str(a);
                            new_string.push_str(&b.to_string());
                            Ok(Value::String(new_string))
                        }
                        (a, Value::String(b)) => {
                            let a_str = a.to_string();
                            let mut new_string = String::with_capacity(a_str.len() + b.len());
                            new_string.push_str(&a_str);
                            new_string.push_str(b);
                            Ok(Value::String(new_string))
                        }
                        (Value::Array(a), Value::Array(b)) => {
                            let mut new_array = Vec::with_capacity(a.len() + b.len());
                            new_array.extend_from_slice(a);
                            new_array.extend_from_slice(b);
                            Ok(Value::Array(new_array))
                        }
                        _ => Err(format!(
                            "add: incompatible types: {} and {}",
                            args[0].type_name(),
                            args[1].type_name()
                        )),
                    }
                }
                "sub" => {
                    if args.len() != 2 {
                        return Err(format!("sub: expected 2 arguments, got {}", args.len()));
                    }
                    match (&args[0], &args[1]) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),
                        _ => Err(format!(
                            "sub: incompatible types: {} and {}",
                            args[0].type_name(),
                            args[1].type_name()
                        )),
                    }
                }
                "mul" => {
                    if args.len() != 2 {
                        return Err(format!("mul: expected 2 arguments, got {}", args.len()));
                    }
                    match (&args[0], &args[1]) {
                        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                        (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
                        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * *b as f64)),
                        _ => Err(format!(
                            "mul: incompatible types: {} and {}",
                            args[0].type_name(),
                            args[1].type_name()
                        )),
                    }
                }
                "div" => {
                    if args.len() != 2 {
                        return Err(format!("div: expected 2 arguments, got {}", args.len()));
                    }
                    match (&args[0], &args[1]) {
                        (Value::Int(a), Value::Int(b)) => {
                            if *b == 0 {
                                return Err("div: division by zero".to_string());
                            }
                            Ok(Value::Int(a / b))
                        }
                        (Value::Float(a), Value::Float(b)) => {
                            if *b == 0.0 {
                                return Err("div: division by zero".to_string());
                            }
                            Ok(Value::Float(a / b))
                        }
                        (Value::Int(a), Value::Float(b)) => {
                            if *b == 0.0 {
                                return Err("div: division by zero".to_string());
                            }
                            Ok(Value::Float(*a as f64 / b))
                        }
                        (Value::Float(a), Value::Int(b)) => {
                            if *b == 0 {
                                return Err("div: division by zero".to_string());
                            }
                            Ok(Value::Float(a / *b as f64))
                        }
                        _ => Err(format!(
                            "div: incompatible types: {} and {}",
                            args[0].type_name(),
                            args[1].type_name()
                        )),
                    }
                }
                "map" => {
                    if args.len() != 2 {
                        return Err(format!("map: expected 2 arguments, got {}", args.len()));
                    }

                    let func = &args[0];
                    match &args[1] {
                        Value::Array(arr) => {
                            let mut result = Vec::new();
                            for item in arr {
                                result.push(func.apply(vec![item.clone()])?);
                            }
                            Ok(Value::Array(result))
                        }
                        _ => Err(format!(
                            "map: second argument must be an array, found {}",
                            args[1].type_name()
                        )),
                    }
                }
                "filter" => {
                    if args.len() != 2 {
                        return Err(format!("filter: expected 2 arguments, got {}", args.len()));
                    }

                    let func = &args[0];
                    match &args[1] {
                        Value::Array(arr) => {
                            let mut result = Vec::new();
                            for item in arr {
                                let res = func.apply(vec![item.clone()])?;
                                if let Value::Bool(true) = res {
                                    result.push(item.clone());
                                }
                            }
                            Ok(Value::Array(result))
                        }
                        _ => Err(format!(
                            "filter: second argument must be an array, found {}",
                            args[1].type_name()
                        )),
                    }
                }
                "reduce" => {
                    if args.len() != 3 {
                        return Err(format!("reduce: expected 3 arguments, got {}", args.len()));
                    }

                    let func = &args[0];
                    let initial = &args[1];
                    match &args[2] {
                        Value::Array(arr) => {
                            let mut acc = initial.clone();
                            for item in arr {
                                acc = func.apply(vec![acc, item.clone()])?;
                            }
                            Ok(acc)
                        }
                        _ => Err(format!(
                            "reduce: third argument must be an array, found {}",
                            args[2].type_name()
                        )),
                    }
                }
                "identity" => {
                    if args.len() != 1 {
                        return Err(format!("identity: expected 1 argument, got {}", args.len()));
                    }

                    Ok(args[0].clone())
                }
                "print" => {
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            print!(" ");
                        }
                        print!("{}", arg);
                    }
                    println!();
                    Ok(Value::Nil)
                }
                "len" => {
                    if args.len() != 1 {
                        return Err(format!("len: expected 1 argument, got {}", args.len()));
                    }
                    match &args[0] {
                        Value::Array(arr) => Ok(Value::Int(arr.len() as i64)),
                        Value::String(s) => Ok(Value::Int(s.len() as i64)),
                        Value::Map(m) => Ok(Value::Int(m.len() as i64)),
                        _ => Err(format!(
                            "len: argument must be a collection, found {}",
                            args[0].type_name()
                        )),
                    }
                }
                _ => Err(format!("Unknown function: {}", name)),
            },
            Value::Curry(curry) => {
                let mut c = curry.borrow_mut();

                let mut all_args = c.applied_args.clone();
                all_args.extend(args);

                if all_args.len() >= c.remaining_params {
                    let func = c.original.clone();

                    let needed_args = all_args[0..c.remaining_params].to_vec();
                    let excess_args = if all_args.len() > c.remaining_params {
                        all_args[c.remaining_params..].to_vec()
                    } else {
                        Vec::new()
                    };

                    let result = func.apply(needed_args)?;

                    if !excess_args.is_empty() {
                        result.apply(excess_args)
                    } else {
                        Ok(result)
                    }
                } else {
                    c.applied_args = all_args;
                    Ok(Value::Curry(Rc::clone(curry)))
                }
            }
            Value::Compose(compose) => {
                let c = compose.borrow();

                let g_result = c.g.apply(args)?;
                c.f.apply(vec![g_result])
            }
            Value::Partial(partial) => {
                let p = partial.borrow();

                let mut arg_idx = 0;
                let mut filled_args = Vec::new();

                for slot in &p.args {
                    if let Some(arg) = slot {
                        filled_args.push(arg.clone());
                    } else if arg_idx < args.len() {
                        filled_args.push(args[arg_idx].clone());
                        arg_idx += 1;
                    } else {
                        let mut new_args = Vec::new();
                        arg_idx = 0;

                        for slot in &p.args {
                            if let Some(arg) = slot {
                                new_args.push(Some(arg.clone()));
                            } else if arg_idx < args.len() {
                                new_args.push(Some(args[arg_idx].clone()));
                                arg_idx += 1;
                            } else {
                                new_args.push(None);
                            }
                        }

                        return Ok(Value::Partial(Rc::new(RefCell::new(PartialFunction {
                            original: p.original.clone(),
                            args: new_args,
                        }))));
                    }
                }

                while arg_idx < args.len() {
                    filled_args.push(args[arg_idx].clone());
                    arg_idx += 1;
                }

                p.original.apply(filled_args)
            }
            Value::Object(obj) => {
                let o = obj.borrow();
                if let Some(Value::Function(method_name)) = o.fields.get("apply") {
                    match method_name.as_str() {
                        "call" => {
                            if matches!(o.typ, ObjectType::Closure(_)) {
                                if let Some(Value::String(func_name)) = o.fields.get("function") {
                                    match crate::stdlib::call_stdlib_function(func_name, &args) {
                                        Ok(result) => Ok(result),
                                        Err(e) => Err(format!("Closure call error: {}", e)),
                                    }
                                } else if let Some(Value::Function(func_name)) =
                                    o.fields.get("function")
                                {
                                    match crate::stdlib::call_stdlib_function(func_name, &args) {
                                        Ok(result) => Ok(result),
                                        Err(e) => Err(format!("Closure call error: {}", e)),
                                    }
                                } else {
                                    Ok(Value::Nil)
                                }
                            } else {
                                Err(format!("Object.apply: object must be a closure"))
                            }
                        }
                        _ => Err(format!("Object.apply: unknown method: {}", method_name)),
                    }
                } else {
                    Err(format!(
                        "Object.apply: object does not support application, found {:?}",
                        o.typ
                    ))
                }
            }
            _ => Err(format!("Not a function: {}", self.type_name())),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Char(c) => write!(f, "'{}'", c),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Object(obj) => {
                let obj = obj.borrow();
                match &obj.typ {
                    ObjectType::Instance(name) => write!(f, "<instance {}>", name),
                    ObjectType::Class(name) => write!(f, "<class {}>", name),
                    ObjectType::Struct(name) => write!(f, "<struct {}>", name),
                    ObjectType::Closure(name) => write!(f, "<function {}>", name),
                    ObjectType::Unit => write!(f, "<unit>"),
                    ObjectType::Curry => write!(f, "<curried function>"),
                    ObjectType::Compose => write!(f, "<composed function>"),
                    ObjectType::Partial => write!(f, "<partial function>"),
                    ObjectType::Lazy => write!(f, "<lazy value>"),
                    ObjectType::Pattern => write!(f, "<pattern>"),
                    ObjectType::Future => write!(f, "<future>"),
                }
            }
            Value::Function(name) => write!(f, "<function {}>", name),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
            Value::Map(map) => {
                write!(f, "{{")?;
                for (i, (key, val)) in map.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, val)?;
                }
                write!(f, "}}")
            }
            Value::Nil => write!(f, "null"),
            Value::Unit => write!(f, "()"),
            Value::Curry(curry) => {
                let c = curry.borrow();
                write!(f, "<curried function with {} args>", c.applied_args.len())
            }
            Value::Compose(_) => write!(f, "<composed function>"),
            Value::Partial(partial) => {
                let p = partial.borrow();
                let holes = p.args.iter().filter(|arg| arg.is_none()).count();
                write!(f, "<partial function with {} holes>", holes)
            }
            Value::Lazy(lazy) => {
                let l = lazy.borrow();
                if l.evaluated {
                    write!(f, "<lazy: evaluated>")
                } else {
                    write!(f, "<lazy: not evaluated>")
                }
            }
            Value::Pattern(_) => write!(f, "<pattern>"),
            Value::Future(future) => {
                let future = future.lock().unwrap();
                if future.completed {
                    if let Some(val) = &future.value {
                        write!(
                            f,
                            "<completed future: {}>",
                            match val {
                                SimpleFutureValue::Int(i) => i.to_string(),
                                SimpleFutureValue::Float(fl) => fl.to_string(),
                                SimpleFutureValue::Bool(b) => b.to_string(),
                                SimpleFutureValue::Char(c) => format!("'{}'", c),
                                SimpleFutureValue::String(s) => format!("\"{}\"", s),
                                SimpleFutureValue::Nil => "null".to_string(),
                            }
                        )
                    } else {
                        write!(f, "<completed future>")
                    }
                } else {
                    write!(f, "<pending future>")
                }
            }
        }
    }
}

impl SimpleFutureValue {
    pub fn to_value(&self) -> Value {
        match self {
            SimpleFutureValue::Int(i) => Value::Int(*i),
            SimpleFutureValue::Float(f) => Value::Float(*f),
            SimpleFutureValue::Bool(b) => Value::Bool(*b),
            SimpleFutureValue::Char(c) => Value::Char(*c),
            SimpleFutureValue::String(s) => Value::String(s.clone()),
            SimpleFutureValue::Nil => Value::Nil,
        }
    }
}

impl std::fmt::Display for SimpleFutureValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleFutureValue::Int(i) => write!(f, "{}", i),
            SimpleFutureValue::Float(fl) => write!(f, "{}", fl),
            SimpleFutureValue::Bool(b) => write!(f, "{}", b),
            SimpleFutureValue::Char(c) => write!(f, "{}", c),
            SimpleFutureValue::String(s) => write!(f, "{}", s),
            SimpleFutureValue::Nil => write!(f, "nil"),
        }
    }
}

impl From<Value> for SimpleFutureValue {
    fn from(value: Value) -> Self {
        match value {
            Value::Int(i) => SimpleFutureValue::Int(i),
            Value::Float(f) => SimpleFutureValue::Float(f),
            Value::Bool(b) => SimpleFutureValue::Bool(b),
            Value::Char(c) => SimpleFutureValue::Char(c),
            Value::String(s) => SimpleFutureValue::String(s),
            Value::Nil => SimpleFutureValue::Nil,
            Value::Unit => SimpleFutureValue::Nil,
            Value::Array(arr) => {
                if arr.is_empty() {
                    SimpleFutureValue::String("[]".to_string())
                } else if arr.len() <= 5 {
                    let elements: Vec<String> = arr.iter().map(|e| format!("{}", e)).collect();
                    SimpleFutureValue::String(format!("[{}]", elements.join(", ")))
                } else {
                    SimpleFutureValue::String(format!("[Array of {} elements]", arr.len()))
                }
            }
            Value::Map(map) => {
                if map.is_empty() {
                    SimpleFutureValue::String("{}".to_string())
                } else if map.len() <= 3 {
                    let entries: Vec<String> =
                        map.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                    SimpleFutureValue::String(format!("{{{}}}", entries.join(", ")))
                } else {
                    SimpleFutureValue::String(format!("{{Map with {} entries}}", map.len()))
                }
            }
            Value::Object(obj) => {
                let obj_ref = obj.borrow();
                match &obj_ref.typ {
                    ObjectType::Instance(class_name) => {
                        SimpleFutureValue::String(format!("<Instance of {}>", class_name))
                    }
                    ObjectType::Class(class_name) => {
                        SimpleFutureValue::String(format!("<Class {}>", class_name))
                    }
                    _ => SimpleFutureValue::String("<Object>".to_string()),
                }
            }
            Value::Function(name) => SimpleFutureValue::String(format!("<Function {}>", name)),
            Value::Curry(_) => SimpleFutureValue::String("<Curried function>".to_string()),
            Value::Compose(_) => SimpleFutureValue::String("<Composed function>".to_string()),
            Value::Partial(_) => SimpleFutureValue::String("<Partial function>".to_string()),
            Value::Lazy(lazy) => {
                let l = lazy.borrow();
                if l.evaluated {
                    if let Some(val) = &l.value {
                        SimpleFutureValue::from(val.clone())
                    } else {
                        SimpleFutureValue::String("<Evaluated lazy>".to_string())
                    }
                } else {
                    SimpleFutureValue::String("<Unevaluated lazy>".to_string())
                }
            }
            Value::Pattern(_) => SimpleFutureValue::String("<Pattern>".to_string()),
            Value::Future(future) => {
                let future_lock = future.lock().unwrap();
                if future_lock.completed {
                    if let Some(val) = &future_lock.value {
                        val.clone()
                    } else {
                        SimpleFutureValue::String("<Completed future>".to_string())
                    }
                } else {
                    SimpleFutureValue::String("<Pending future>".to_string())
                }
            }
        }
    }
}
