use crate::error::Span;
use std::fmt;

/// Type system for Vanua language
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Built-in primitive types
    Primitive(PrimitiveType, Span),

    /// Array type, e.g., Array<Int>
    Array(Box<Type>, Span),

    /// Map type, e.g., Map<String, Int>
    Map(Box<Type>, Box<Type>, Span),

    /// Tuple type, e.g., (Int, String, Bool, etc)
    Tuple(Vec<Type>, Span),

    /// Nullable type, e.g., Int?
    Nullable(Box<Type>, Span),

    /// Named user-defined type (class, struct, interface, trait)
    Named(String, Vec<Type>, Span),

    /// Function type with parameter types and return type
    Function(Vec<Type>, Box<Type>, Span),

    /// Unit type - fundamental base object type for inheritance hierarchy
    Unit(Span),

    /// Any type (used for generics and inference)
    Any(Span),

    /// Type parameter (for generics)
    TypeParam(String, Span),

    /// Pointer type, e.g., *Int
    Pointer(Box<Type>, Span),

    /// Reference type, e.g., &Int
    Reference(Box<Type>, Span),

    /// Future type, for async operations, e.g., Future<Int>
    Future(Box<Type>, Span),

    /// Unknown type (used during type inference)
    Unknown(Span),
}

/// Primitive types in Vanua
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    Char,
    String,
    Nothing,
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Primitive(_, span) => *span,
            Type::Array(_, span) => *span,
            Type::Map(_, _, span) => *span,
            Type::Tuple(_, span) => *span,
            Type::Nullable(_, span) => *span,
            Type::Named(_, _, span) => *span,
            Type::Function(_, _, span) => *span,
            Type::Unit(span) => *span,
            Type::Any(span) => *span,
            Type::TypeParam(_, span) => *span,
            Type::Pointer(_, span) => *span,
            Type::Reference(_, span) => *span,
            Type::Future(_, span) => *span,
            Type::Unknown(span) => *span,
        }
    }

    /// Creates a primitive type with a default span
    pub fn primitive(prim_type: PrimitiveType) -> Self {
        Type::Primitive(prim_type, Span::default())
    }

    /// Creates an array type with a default span
    pub fn array(elem_type: Type) -> Self {
        Type::Array(Box::new(elem_type), Span::default())
    }

    /// Creates a nullable type with a default span
    pub fn nullable(inner_type: Type) -> Self {
        Type::Nullable(Box::new(inner_type), Span::default())
    }

    /// Creates a pointer type with a default span
    pub fn pointer(target_type: Type) -> Self {
        Type::Pointer(Box::new(target_type), Span::default())
    }

    /// Creates a reference type with a default span
    pub fn reference(target_type: Type) -> Self {
        Type::Reference(Box::new(target_type), Span::default())
    }

    /// Checks if the type is a primitive type
    pub fn is_primitive(&self) -> bool {
        matches!(self, Type::Primitive(_, _))
    }

    /// Checks if the type is numeric (Int or Float)
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::Primitive(PrimitiveType::Int, _) | Type::Primitive(PrimitiveType::Float, _)
        )
    }

    /// Checks if the type is a unit type (void)
    pub fn is_unit(&self) -> bool {
        matches!(self, Type::Unit(_))
    }

    /// Checks if the type is nullable
    pub fn is_nullable(&self) -> bool {
        matches!(self, Type::Nullable(_, _))
    }

    /// Checks if the type is a pointer
    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_, _))
    }

    /// Checks if the type is a reference
    pub fn is_reference(&self) -> bool {
        matches!(self, Type::Reference(_, _))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Primitive(prim_type, _) => write!(f, "{}", prim_type),
            Type::Array(elem_type, _) => write!(f, "Array<{}>", elem_type),
            Type::Map(key_type, value_type, _) => write!(f, "Map<{}, {}>", key_type, value_type),
            Type::Tuple(types, _) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Nullable(inner_type, _) => write!(f, "{}?", inner_type),
            Type::Named(name, type_args, _) => {
                write!(f, "{}", name)?;
                if !type_args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Type::Function(param_types, return_type, _) => {
                write!(f, "(")?;
                for (i, param) in param_types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", return_type)
            }
            Type::Unit(_) => write!(f, "Unit"),
            Type::Any(_) => write!(f, "Any"),
            Type::TypeParam(name, _) => write!(f, "{}", name),
            Type::Pointer(inner_type, _) => write!(f, "*{}", inner_type),
            Type::Reference(inner_type, _) => write!(f, "&{}", inner_type),
            Type::Future(inner_type, _) => write!(f, "Future<{}>", inner_type),
            Type::Unknown(_) => write!(f, "Unknown"),
        }
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveType::Int => write!(f, "Int"),
            PrimitiveType::Float => write!(f, "Float"),
            PrimitiveType::Bool => write!(f, "Bool"),
            PrimitiveType::Char => write!(f, "Char"),
            PrimitiveType::String => write!(f, "String"),
            PrimitiveType::Nothing => write!(f, "Nothing"),
        }
    }
}

/// Type parameter with optional bounds
#[derive(Debug, Clone, PartialEq)]
pub struct TypeParameter {
    pub name: String,
    pub bounds: Vec<Type>,
    pub span: Span,
}

impl TypeParameter {
    pub fn new(name: String, bounds: Vec<Type>, span: Span) -> Self {
        Self { name, bounds, span }
    }
}

impl fmt::Display for TypeParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.bounds.is_empty() {
            write!(f, " : ")?;
            for (i, bound) in self.bounds.iter().enumerate() {
                if i > 0 {
                    write!(f, " + ")?;
                }
                write!(f, "{}", bound)?;
            }
        }
        Ok(())
    }
}
