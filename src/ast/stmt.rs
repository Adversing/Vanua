use super::{Expression, Parameter, Type, TypeParameter};
use crate::error::Span;
use std::fmt;

/// Statement types in the Vanua language
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Expression statement
    Expression(Expression, Span),

    /// Variable declaration
    VarDecl(VarDeclaration, Span),

    /// Block of statements
    Block(Vec<Statement>, Span),

    /// If statement
    If(Expression, Box<Statement>, Option<Box<Statement>>, Span),

    /// While loop
    While(Expression, Box<Statement>, Span),

    /// For loop
    For(
        String,
        Option<Type>,
        Expression,
        Expression,
        Option<Expression>,
        Box<Statement>,
        Span,
    ),

    /// For-in loop
    ForIn(String, Expression, Box<Statement>, Span),

    /// Return statement
    Return(Option<Expression>, Span),

    /// Break statement
    Break(Span),

    /// Continue statement
    Continue(Span),

    /// Defer statement
    Defer(Box<Statement>, Span),

    /// Unsafe block statement
    UnsafeBlock(Vec<Statement>, Span),

    /// Explicit memory allocation with automated management
    Alloc(String, Type, Option<Expression>, Span),

    /// Explicit (but controlled) memory deallocation
    Free(Expression, Span),

    /// Await expression statement
    Await(Expression, Span),
}

/// Declaration types in the Vanua language
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Function declaration
    Function(FunctionDeclaration),

    /// Class declaration
    Class(ClassDeclaration),

    /// Trait declaration
    Trait(TraitDeclaration),

    /// Struct declaration
    Struct(StructDeclaration),

    /// Implementation block
    Impl(ImplDeclaration),
}

/// Variable declaration
#[derive(Debug, Clone, PartialEq)]
pub struct VarDeclaration {
    pub name: String,
    pub var_type: Option<Type>,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
    pub visibility: Visibility,
    pub span: Span,
}

/// Function declaration
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Option<Statement>,
    pub type_params: Vec<TypeParameter>,
    pub visibility: Visibility,
    pub is_async: bool,
    pub method_modifier: MethodModifier,
    pub span: Span,
}

/// Class declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ClassDeclaration {
    pub name: String,
    pub superclasses: Vec<Type>,
    pub implements: Vec<Type>,
    pub type_params: Vec<TypeParameter>,
    pub methods: Vec<FunctionDeclaration>,
    pub fields: Vec<VarDeclaration>,
    pub visibility: Visibility,
    pub constructors: Vec<ConstructorDeclaration>,
    pub span: Span,
}

/// Constructor declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorDeclaration {
    pub params: Vec<Parameter>,
    pub body: Statement,
    pub visibility: Visibility,
    pub span: Span,
}

/// Trait declaration
#[derive(Debug, Clone, PartialEq)]
pub struct TraitDeclaration {
    pub name: String,
    pub extends: Vec<Type>,
    pub type_params: Vec<TypeParameter>,
    pub methods: Vec<FunctionDeclaration>,
    pub required_methods: Vec<FunctionDeclaration>,
    pub visibility: Visibility,
    pub span: Span,
}

/// Struct declaration
#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclaration {
    pub name: String,
    pub type_params: Vec<TypeParameter>,
    pub fields: Vec<VarDeclaration>,
    pub visibility: Visibility,
    pub span: Span,
}

/// Implementation block
#[derive(Debug, Clone, PartialEq)]
pub struct ImplDeclaration {
    pub trait_type: Option<Type>,
    pub for_type: Type,
    pub type_params: Vec<TypeParameter>,
    pub methods: Vec<FunctionDeclaration>,
    pub span: Span,
}

/// Visibility modifier
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

/// Method modifier for overriding behavior
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodModifier {
    /// Method can be overridden by subclasses
    Open,
    /// Method overrides a parent method
    Override,
    /// Method cannot be overridden (default for non-open methods)
    Final,
    /// No special modifier (default)
    None,
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Expression(_, span) => *span,
            Statement::VarDecl(_, span) => *span,
            Statement::Block(_, span) => *span,
            Statement::If(_, _, _, span) => *span,
            Statement::While(_, _, span) => *span,
            Statement::For(_, _, _, _, _, _, span) => *span,
            Statement::ForIn(_, _, _, span) => *span,
            Statement::Return(_, span) => *span,
            Statement::Break(span) => *span,
            Statement::Continue(span) => *span,
            Statement::Defer(_, span) => *span,
            Statement::UnsafeBlock(_, span) => *span,
            Statement::Alloc(_, _, _, span) => *span,
            Statement::Free(_, span) => *span,
            Statement::Await(_, span) => *span,
        }
    }
}

impl Declaration {
    pub fn span(&self) -> Span {
        match self {
            Declaration::Function(f) => f.span,
            Declaration::Class(c) => c.span,
            Declaration::Trait(t) => t.span,
            Declaration::Struct(s) => s.span,
            Declaration::Impl(i) => i.span,
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Declaration::Function(f) => &f.name,
            Declaration::Class(c) => &c.name,
            Declaration::Trait(t) => &t.name,
            Declaration::Struct(s) => &s.name,
            Declaration::Impl(_) => "", // impl blocks have no name
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expression(expr, _) => write!(f, "{};", expr),
            Statement::VarDecl(var_decl, _) => {
                let kind = if var_decl.is_mutable { "var" } else { "val" };
                write!(f, "{} {}", kind, var_decl.name)?;
                if let Some(typ) = &var_decl.var_type {
                    write!(f, ": {}", typ)?;
                }
                if let Some(init) = &var_decl.initializer {
                    write!(f, " = {}", init)?;
                }
                write!(f, ";")
            }
            Statement::Block(stmts, _) => {
                writeln!(f, "{{")?;
                for stmt in stmts {
                    writeln!(f, "  {}", stmt)?;
                }
                write!(f, "}}")
            }
            Statement::If(cond, then_stmt, else_stmt, _) => {
                write!(f, "if ({}) {}", cond, then_stmt)?;
                if let Some(else_stmt) = else_stmt {
                    write!(f, " else {}", else_stmt)?;
                }
                Ok(())
            }
            Statement::While(cond, body, _) => {
                write!(f, "while ({}) {}", cond, body)
            }
            Statement::For(var, var_type, start, end, step, body, _) => {
                write!(f, "for ({}:", var)?;
                if let Some(typ) = var_type {
                    write!(f, " {}", typ)?;
                }
                write!(f, " in {} to {}", start, end)?;
                if let Some(step_expr) = step {
                    write!(f, " step {}", step_expr)?;
                }
                write!(f, ") {}", body)
            }
            Statement::ForIn(var, collection, body, _) => {
                write!(f, "for ({} in {}) {}", var, collection, body)
            }
            Statement::Return(expr, _) => {
                write!(f, "return")?;
                if let Some(expr) = expr {
                    write!(f, " {}", expr)?;
                }
                write!(f, ";")
            }
            Statement::Break(_) => write!(f, "break;"),
            Statement::Continue(_) => write!(f, "continue;"),
            Statement::Defer(stmt, _) => write!(f, "defer {}", stmt),
            Statement::UnsafeBlock(stmts, _) => {
                writeln!(f, "unsafe {{")?;
                for stmt in stmts {
                    writeln!(f, "  {}", stmt)?;
                }
                write!(f, "}}")
            }
            Statement::Alloc(name, typ, size, _) => {
                write!(f, "alloc {}: {}", name, typ)?;
                if let Some(size_expr) = size {
                    write!(f, "[{}]", size_expr)?;
                }
                write!(f, ";")
            }
            Statement::Free(expr, _) => write!(f, "free {};", expr),
            Statement::Await(expr, _) => write!(f, "await {};", expr),
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Declaration::Function(func) => {
                write!(f, "fun {}(", func.name)?;
                for (i, param) in func.params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ")")?;
                if let Some(ret_type) = &func.return_type {
                    write!(f, ": {}", ret_type)?;
                }
                if let Some(body) = &func.body {
                    write!(f, " {}", body)
                } else {
                    write!(f, ";")
                }
            }
            Declaration::Class(class) => {
                write!(f, "class {}", class.name)?;
                if !class.type_params.is_empty() {
                    write!(f, "<")?;
                    for (i, tp) in class.type_params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?
                        }
                        write!(f, "{}", tp)?;
                    }
                    write!(f, ">")?;
                }
                if !class.superclasses.is_empty() {
                    write!(f, " : ")?;
                    for (i, superclass) in class.superclasses.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?
                        }
                        write!(f, "{}", superclass)?;
                    }
                }
                if !class.implements.is_empty() {
                    write!(f, " impl ")?;
                    for (i, intf) in class.implements.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?
                        }
                        write!(f, "{}", intf)?;
                    }
                }
                writeln!(f, " {{")?;
                for field in &class.fields {
                    writeln!(f, "  {}", field)?;
                }
                for _ in &class.constructors {
                    writeln!(f, "  constructor(...) {{ ... }}")?;
                }
                for method in &class.methods {
                    writeln!(f, "  {}", method)?;
                }
                write!(f, "}}")
            }

            Declaration::Trait(trait_) => {
                write!(f, "trait {}", trait_.name)?;
                if !trait_.type_params.is_empty() {
                    write!(f, "<")?;
                    for (i, tp) in trait_.type_params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?
                        }
                        write!(f, "{}", tp)?;
                    }
                    write!(f, ">")?;
                }
                if !trait_.extends.is_empty() {
                    write!(f, " extends ")?;
                    for (i, ext) in trait_.extends.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?
                        }
                        write!(f, "{}", ext)?;
                    }
                }
                writeln!(f, " {{")?;
                for method in &trait_.methods {
                    writeln!(f, "  {}", method)?;
                }
                write!(f, "}}")
            }
            Declaration::Struct(struct_) => {
                write!(f, "struct {}", struct_.name)?;
                if !struct_.type_params.is_empty() {
                    write!(f, "<")?;
                    for (i, tp) in struct_.type_params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?
                        }
                        write!(f, "{}", tp)?;
                    }
                    write!(f, ">")?;
                }
                writeln!(f, " {{")?;
                for field in &struct_.fields {
                    writeln!(f, "  {}", field)?;
                }
                write!(f, "}}")
            }
            Declaration::Impl(impl_) => {
                write!(f, "impl")?;
                if let Some(trait_) = &impl_.trait_type {
                    write!(f, " {} for", trait_)?;
                }
                write!(f, " {}", impl_.for_type)?;
                if !impl_.type_params.is_empty() {
                    write!(f, "<")?;
                    for (i, tp) in impl_.type_params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?
                        }
                        write!(f, "{}", tp)?;
                    }
                    write!(f, ">")?;
                }
                writeln!(f, " {{")?;
                for method in &impl_.methods {
                    writeln!(f, "  {}", method)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Visibility::Public => write!(f, "pub"),
            Visibility::Private => write!(f, "priv"),
        }
    }
}

impl fmt::Display for VarDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let kind = if self.is_mutable { "var" } else { "val" };

        if self.visibility == Visibility::Public {
            write!(f, "{} ", self.visibility)?;
        }

        write!(f, "{} {}", kind, self.name)?;

        if let Some(typ) = &self.var_type {
            write!(f, ": {}", typ)?;
        }

        if let Some(init) = &self.initializer {
            write!(f, " = {}", init)?;
        }

        write!(f, ";")
    }
}

impl fmt::Display for FunctionDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.visibility == Visibility::Public {
            write!(f, "{} ", self.visibility)?;
        }

        write!(f, "fun {}(", self.name)?;

        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?
            }
            write!(f, "{}", param.name)?;
            if let Some(typ) = &param.param_type {
                write!(f, ": {}", typ)?;
            }
        }

        write!(f, ")")?;

        if let Some(ret_type) = &self.return_type {
            write!(f, ": {}", ret_type)?;
        }

        if let Some(body) = &self.body {
            write!(f, " {}", body)
        } else {
            write!(f, ";")
        }
    }
}
