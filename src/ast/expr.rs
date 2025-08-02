use super::Type;
use crate::error::Span;
use std::fmt;

/// Expression types in the Vanua language
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Literal values (numbers, strings, etc.)
    Literal(Literal, Span),

    /// Variable references
    Variable(String, Span),

    /// Binary operations (a + b, a * b, etc.)
    Binary(Box<Expression>, BinaryOp, Box<Expression>, Span),

    /// Unary operations (!a, -b, etc.)
    Unary(UnaryOp, Box<Expression>, Span),

    /// Function calls
    Call(Box<Expression>, Vec<Expression>, Span),

    /// Property access (obj.prop)
    Property(Box<Expression>, String, Span),

    /// Method calls (obj.method())
    Method(Box<Expression>, String, Vec<Expression>, Span),

    /// Array indexing (arr[idx])
    Index(Box<Expression>, Box<Expression>, Span),

    /// Array literals ([1, 2, 3])
    Array(Vec<Expression>, Span),

    /// Map literals ({key: value})
    Map(Vec<(Expression, Expression)>, Span),

    /// Anonymous functions
    Lambda(Vec<Parameter>, Box<Expression>, Option<Type>, Span),

    /// Conditional expressions (if a then b else c)
    If(
        Box<Expression>,
        Box<Expression>,
        Option<Box<Expression>>,
        Span,
    ),

    /// Ternary operator (condition ? true_expr : false_expr)
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>, Span),

    /// Assignment
    Assign(Box<Expression>, Box<Expression>, Span),

    /// Compound assignment (+=, -=, etc.)
    CompoundAssign(Box<Expression>, BinaryOp, Box<Expression>, Span),

    /// Null-safe property access (obj?.prop)
    SafeProperty(Box<Expression>, String, Span),

    /// Null-safe method call (obj?.method())
    SafeMethod(Box<Expression>, String, Vec<Expression>, Span),

    /// Elvis operator (a ?: b)
    Elvis(Box<Expression>, Box<Expression>, Span),

    /// Type cast (value as Type)
    Cast(Box<Expression>, Type, Span),

    /// Type check (value is Type)
    Is(Box<Expression>, Type, Span),

    /// This reference
    This(Span),

    /// Super reference
    Super(String, Span),

    /// Struct literal instantiation: MyStruct{field=value, field2=value2}
    StructLiteral(String, Vec<(String, Expression)>, Span),

    /// Function currying (automatic conversion of multi-argument function to chain of single-argument functions)
    Curry(Box<Expression>, Span),

    /// Function composition (f ∘ g)(x) = f(g(x))
    Compose(Box<Expression>, Box<Expression>, Span),

    /// Partial application of a function (providing only some arguments)
    PartialApply(Box<Expression>, Vec<Option<Expression>>, Span),

    /// Pattern matching expression
    Match(Box<Expression>, Vec<(Pattern, Expression)>, Span),

    /// Lazy evaluation (expression is evaluated only when needed)
    Lazy(Box<Expression>, Span),

    /// Delete expression - memory deallocation
    Delete(Box<Expression>, Span),

    /// Sizeof expression - size of a type
    SizeOf(Type, Span),

    /// Dereference expression - pointer dereferencing
    Dereference(Box<Expression>, Span),

    /// Address-of expression - obtaining a reference
    AddressOf(Box<Expression>, Span),

    /// Await expression - waiting for an asynchronous operation
    Await(Box<Expression>, Span),

    /// String interpolation expression - "Hello #{name}!"
    InterpolatedString(Vec<InterpolationPart>, Span),
}

/// Parts of an interpolated string
#[derive(Debug, Clone, PartialEq)]
pub enum InterpolationPart {
    /// String literal part
    String(String),
    /// Expression part (inside #{})
    Expression(Expression),
}

/// Literal value types
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Null,
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Eq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,

    // Logical
    And,
    Or,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    BitShl,
    BitShr,

    // Other
    NullCoalesce,

    // Function composition operator
    Compose,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
    BitNot,
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,

    // Nullability check operator (value?)
    NullabilityCheck,

    // Currying operator
    Curry,
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: Option<Type>,
    pub default_value: Option<Expression>,
    pub is_mutable: bool,
    pub span: Span,
}

/// Pattern for pattern matching
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard pattern (_)
    Wildcard(Span),

    /// Literal pattern (42, "hello", etc.)
    Literal(Literal, Span),

    /// Variable binding pattern (x)
    Variable(String, Span),

    /// Destructuring pattern for arrays
    Array(Vec<Pattern>, Span),

    /// Destructuring pattern for objects
    Object(Vec<(String, Pattern)>, Span),

    /// Type test pattern (is Type)
    TypeTest(Type, Span),

    /// OR pattern (a | b)
    Or(Box<Pattern>, Box<Pattern>, Span),

    /// AND pattern (a & b)
    And(Box<Pattern>, Box<Pattern>, Span),

    /// Guard pattern (pattern if condition)
    Guard(Box<Pattern>, Box<Expression>, Span),

    /// Tuple pattern (a, b, c)
    Tuple(Vec<Pattern>, Span),

    /// Record pattern {a: pattern, b: pattern}
    Record(Vec<(String, Pattern)>, Span),
}

impl Parameter {
    pub fn new(
        name: String,
        param_type: Option<Type>,
        default_value: Option<Expression>,
        is_mutable: bool,
        span: Span,
    ) -> Self {
        Self {
            name,
            param_type,
            default_value,
            is_mutable,
            span,
        }
    }
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Literal(_, span) => *span,
            Expression::Variable(_, span) => *span,
            Expression::Binary(_, _, _, span) => *span,
            Expression::Unary(_, _, span) => *span,
            Expression::Call(_, _, span) => *span,
            Expression::Property(_, _, span) => *span,
            Expression::Method(_, _, _, span) => *span,
            Expression::Index(_, _, span) => *span,
            Expression::Array(_, span) => *span,
            Expression::Map(_, span) => *span,
            Expression::Lambda(_, _, _, span) => *span,
            Expression::If(_, _, _, span) => *span,
            Expression::Ternary(_, _, _, span) => *span,
            Expression::Assign(_, _, span) => *span,
            Expression::CompoundAssign(_, _, _, span) => *span,
            Expression::SafeProperty(_, _, span) => *span,
            Expression::SafeMethod(_, _, _, span) => *span,
            Expression::Elvis(_, _, span) => *span,
            Expression::Cast(_, _, span) => *span,
            Expression::Is(_, _, span) => *span,
            Expression::This(span) => *span,
            Expression::Super(_, span) => *span,
            Expression::StructLiteral(_, _, span) => *span,
            Expression::Curry(_, span) => *span,
            Expression::Compose(_, _, span) => *span,
            Expression::PartialApply(_, _, span) => *span,
            Expression::Match(_, _, span) => *span,
            Expression::Lazy(_, span) => *span,
            Expression::Delete(_, span) => *span,
            Expression::SizeOf(_, span) => *span,
            Expression::Dereference(_, span) => *span,
            Expression::AddressOf(_, span) => *span,
            Expression::Await(_, span) => *span,
            Expression::InterpolatedString(_, span) => *span,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Literal(lit, _) => write!(f, "{}", lit),
            Expression::Variable(name, _) => write!(f, "{}", name),
            Expression::Binary(left, op, right, _) => write!(f, "({} {} {})", left, op, right),
            Expression::Unary(op, expr, _) => write!(f, "({} {})", op, expr),
            Expression::Call(callee, args, _) => {
                write!(f, "{}(", callee)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expression::Property(obj, name, _) => write!(f, "{}.{}", obj, name),
            Expression::Method(obj, name, args, _) => {
                write!(f, "{}.{}(", obj, name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expression::Index(array, index, _) => write!(f, "{}[{}]", array, index),
            Expression::Array(elements, _) => {
                write!(f, "[")?;
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", element)?;
                }
                write!(f, "]")
            }
            Expression::Map(entries, _) => {
                write!(f, "{{")?;
                for (i, (key, value)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            Expression::Lambda(params, body, return_type, _) => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param.name)?;
                    if let Some(typ) = &param.param_type {
                        write!(f, ": {}", typ)?;
                    }
                }
                write!(f, ")")?;
                if let Some(ret_type) = return_type {
                    write!(f, " -> {}", ret_type)?;
                }
                write!(f, " {}", body)
            }
            Expression::If(cond, then, els, _) => {
                write!(f, "if {} then {}", cond, then)?;
                if let Some(els_expr) = els {
                    write!(f, " else {}", els_expr)?;
                }
                Ok(())
            }
            Expression::Ternary(cond, true_expr, false_expr, _) => {
                write!(f, "{} ? {} : {}", cond, true_expr, false_expr)
            }
            Expression::Assign(target, value, _) => write!(f, "{} = {}", target, value),
            Expression::CompoundAssign(target, op, value, _) => {
                write!(f, "{} {}= {}", target, op, value)
            }
            Expression::SafeProperty(obj, name, _) => write!(f, "{}?.{}", obj, name),
            Expression::SafeMethod(obj, name, args, _) => {
                write!(f, "{}?.{}(", obj, name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expression::Elvis(left, right, _) => write!(f, "{} ?: {}", left, right),
            Expression::Cast(expr, typ, _) => write!(f, "{} as {}", expr, typ),
            Expression::Is(expr, typ, _) => write!(f, "{} is {}", expr, typ),
            Expression::This(_) => write!(f, "this"),
            Expression::Super(_, _) => write!(f, "super"),
            Expression::StructLiteral(name, fields, _) => {
                write!(f, "{} {{ ", name)?;
                for (i, (field_name, field_value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{} = {}", field_name, field_value)?;
                }
                write!(f, " }}")
            }
            Expression::Curry(func, _) => write!(f, "curry({})", func),
            Expression::Compose(f_expr, g_expr, _) => {
                write!(f, "{} ∘ {}", f_expr, g_expr)
            }
            Expression::PartialApply(func, args, _) => {
                write!(f, "{}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    match arg {
                        Some(expr) => write!(f, "{}", expr)?,
                        None => write!(f, "_")?,
                    }
                }
                write!(f, ")")
            }
            Expression::Match(expr, cases, _) => {
                write!(f, "match {} {{", expr)?;
                for (pattern, result) in cases {
                    write!(f, "{} => {}, ", pattern, result)?;
                }
                write!(f, "}}")
            }
            Expression::Lazy(expr, _) => write!(f, "lazy({})", expr),
            Expression::Delete(_, _) => write!(f, "delete"),
            Expression::SizeOf(_, _) => write!(f, "sizeof"),
            Expression::Dereference(expr, _) => write!(f, "*({})", expr),
            Expression::AddressOf(expr, _) => write!(f, "&({})", expr),
            Expression::Await(expr, _) => write!(f, "await {}", expr),
            Expression::InterpolatedString(parts, _) => {
                write!(f, "\"")?;
                for part in parts {
                    match part {
                        InterpolationPart::String(s) => write!(f, "{}", s)?,
                        InterpolationPart::Expression(expr) => write!(f, "#{{{}}}", expr)?,
                    }
                }
                write!(f, "\"")
            }
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Char(c) => write!(f, "'{}'", c),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Null => write!(f, "null"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Mod => write!(f, "%"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Lte => write!(f, "<="),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Gte => write!(f, ">="),
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
            BinaryOp::BitAnd => write!(f, "&"),
            BinaryOp::BitOr => write!(f, "|"),
            BinaryOp::BitXor => write!(f, "^"),
            BinaryOp::BitShl => write!(f, "<<"),
            BinaryOp::BitShr => write!(f, ">>"),
            BinaryOp::NullCoalesce => write!(f, "??"),
            BinaryOp::Compose => write!(f, "∘"),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Negate => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
            UnaryOp::BitNot => write!(f, "~"),
            UnaryOp::PreIncrement => write!(f, "++"),
            UnaryOp::PreDecrement => write!(f, "--"),
            UnaryOp::PostIncrement => write!(f, "++"),
            UnaryOp::PostDecrement => write!(f, "--"),
            UnaryOp::NullabilityCheck => write!(f, "?"),
            UnaryOp::Curry => write!(f, "curry"),
        }
    }
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_mutable {
            write!(f, "mut ")?;
        }
        write!(f, "{}", self.name)?;
        if let Some(ty) = &self.param_type {
            write!(f, ": {}", ty)?;
        }
        if let Some(default) = &self.default_value {
            write!(f, " = {}", default)?;
        }
        Ok(())
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Wildcard(_) => write!(f, "_"),
            Pattern::Literal(lit, _) => write!(f, "{}", lit),
            Pattern::Variable(name, _) => write!(f, "{}", name),
            Pattern::Array(patterns, _) => {
                write!(f, "[")?;
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", pattern)?;
                }
                write!(f, "]")
            }
            Pattern::Object(fields, _) => {
                write!(f, "{{")?;
                for (i, (name, pattern)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, pattern)?;
                }
                write!(f, "}}")
            }
            Pattern::TypeTest(typ, _) => write!(f, "is {}", typ),
            Pattern::Or(left, right, _) => write!(f, "{} | {}", left, right),
            Pattern::And(left, right, _) => write!(f, "{} & {}", left, right),
            Pattern::Guard(pattern, condition, _) => write!(f, "{} if {}", pattern, condition),
            Pattern::Tuple(_, _) => write!(f, "tuple"),
            Pattern::Record(_, _) => write!(f, "record"),
        }
    }
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard(span) => *span,
            Pattern::Literal(_, span) => *span,
            Pattern::Variable(_, span) => *span,
            Pattern::Array(_, span) => *span,
            Pattern::Object(_, span) => *span,
            Pattern::TypeTest(_, span) => *span,
            Pattern::Or(_, _, span) => *span,
            Pattern::And(_, _, span) => *span,
            Pattern::Guard(_, _, span) => *span,
            Pattern::Tuple(_, span) => *span,
            Pattern::Record(_, span) => *span,
        }
    }
}
