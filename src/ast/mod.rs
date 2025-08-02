mod expr;
mod stmt;
mod types;

pub use expr::*;
pub use stmt::*;
pub use types::*;

use crate::error::Span;
use std::fmt;

/// Complete program declaration
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub package_decl: Option<Package>,
    pub imports: Vec<Import>,
    pub declarations: Vec<Declaration>,
    pub span: Span,
}

impl Program {
    pub fn new(
        package_decl: Option<Package>,
        imports: Vec<Import>,
        declarations: Vec<Declaration>,
        span: Span,
    ) -> Self {
        Self {
            package_decl,
            imports,
            declarations,
            span,
        }
    }
}

/// Represents a package declaration
#[derive(Debug, Clone, PartialEq)]
pub struct Package {
    pub name: String,
    pub span: Span,
}

impl Package {
    pub fn new(name: String, span: Span) -> Self {
        Self { name, span }
    }
}

/// Represents an import declaration
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub path: Vec<String>,
    pub alias: Option<String>,
    pub span: Span,
}

impl Import {
    pub fn new(path: Vec<String>, alias: Option<String>, span: Span) -> Self {
        Self { path, alias, span }
    }
}

/// Generic node type that all AST nodes can convert to
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(Program),
    Package(Package),
    Import(Import),
    Declaration(Declaration),
    Statement(Statement),
    Expression(Expression),
    Type(Type),
}

impl Node {
    pub fn span(&self) -> Span {
        match self {
            Node::Program(n) => n.span,
            Node::Package(n) => n.span,
            Node::Import(n) => n.span,
            Node::Declaration(n) => n.span(),
            Node::Statement(n) => n.span(),
            Node::Expression(n) => n.span(),
            Node::Type(n) => n.span(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Program {{")?;

        if let Some(pkg) = &self.package_decl {
            writeln!(f, "  Package: {}", pkg.name)?;
        }

        if !self.imports.is_empty() {
            writeln!(f, "  Imports:")?;
            for import in &self.imports {
                writeln!(f, "    {}", import.path.join("."))?;
            }
        }

        if !self.declarations.is_empty() {
            writeln!(f, "  Declarations:")?;
            for decl in &self.declarations {
                write!(f, "    {}", decl)?;
            }
        }

        write!(f, "}}")
    }
}

impl fmt::Display for Import {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "import {}", self.path.join("."))?;
        if let Some(alias) = &self.alias {
            write!(f, " as {}", alias)?;
        }
        Ok(())
    }
}
