use std::fmt;
use thiserror::Error;

/// Information about position in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

impl Span {
    pub fn new(line: usize, column: usize, length: usize) -> Self {
        Self {
            line,
            column,
            length,
        }
    }

    pub fn default() -> Self {
        Self {
            line: 0,
            column: 0,
            length: 0,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}-{}",
            self.line,
            self.column,
            self.column + self.length
        )
    }
}

/// Vanua compiler error
#[derive(Error, Debug, Clone)]
pub enum VanuaError {
    /// I/O error
    #[error("I/O Error: {message}")]
    IoError { message: String },

    /// Lexical error (during tokenization)
    #[error("Lexical error: {message} at line {line}, column {column}")]
    LexError {
        message: String,
        line: usize,
        column: usize,
    },

    /// Parsing error (during AST construction)
    #[error("Parsing error: {message} at line {line}, column {column}")]
    ParseError {
        message: String,
        line: usize,
        column: usize,
    },

    /// Type error (during type checking)
    #[error(
        "Type error at line {line}, column {column}: {message}, expected {expected}, found {found}"
    )]
    TypeError {
        message: String,
        expected: String,
        found: String,
        line: usize,
        column: usize,
    },

    /// Code generation error
    #[error("Code generation error: {message} at line {line}, column {column}")]
    CodegenError {
        message: String,
        line: usize,
        column: usize,
    },

    /// Runtime error in the VM
    #[error("Runtime error: {message}")]
    RuntimeError {
        message: String,
        #[source]
        cause: Option<Box<VanuaError>>,
    },

    /// Undefined variable error
    #[error("Undefined variable '{name}' at line {line}, column {column}")]
    UndefinedVariable {
        name: String,
        line: usize,
        column: usize,
    },

    /// Undefined function error
    #[error("Undefined function '{name}' at line {line}, column {column}")]
    UndefinedFunction {
        name: String,
        line: usize,
        column: usize,
    },

    /// Undefined class error
    #[error("Undefined class '{name}' at line {line}, column {column}")]
    UndefinedClass {
        name: String,
        line: usize,
        column: usize,
    },

    /// Invalid operation error
    #[error("Invalid operation: {message}")]
    InvalidOperation { message: String },

    /// Generic error
    #[error("Error: {message}")]
    GenericError { message: String },

    /// Pattern matching error
    #[error("Pattern matching error: {message} at line {line}, column {column}")]
    PatternMatchError {
        message: String,
        line: usize,
        column: usize,
    },

    /// Partial application error
    #[error("Partial application error: {message}")]
    PartialApplicationError { message: String },

    /// Currying error
    #[error("Currying error: {message}")]
    CurryingError { message: String },

    /// Function composition error
    #[error("Function composition error: {message}")]
    CompositionError { message: String },

    /// Lazy evaluation error
    #[error("Lazy evaluation error: {message}")]
    LazyEvaluationError { message: String },
}

/// Result type for Vanua
pub type VanuaResult<T> = Result<T, VanuaError>;

impl VanuaError {
    /// Creates a lexical error
    pub fn lexer_error(message: impl Into<String>, span: Span) -> Self {
        VanuaError::LexError {
            message: message.into(),
            line: span.line,
            column: span.column,
        }
    }

    /// Creates a parsing error
    pub fn parser_error(message: impl Into<String>, span: Span) -> Self {
        VanuaError::ParseError {
            message: message.into(),
            line: span.line,
            column: span.column,
        }
    }

    /// Creates a type error
    pub fn type_error(
        message: impl Into<String>,
        expected: impl Into<String>,
        found: impl Into<String>,
        span: Span,
    ) -> Self {
        VanuaError::TypeError {
            message: message.into(),
            expected: expected.into(),
            found: found.into(),
            line: span.line,
            column: span.column,
        }
    }

    /// Creates a codegen error
    pub fn codegen_error(message: impl Into<String>, span: Span) -> Self {
        VanuaError::CodegenError {
            message: message.into(),
            line: span.line,
            column: span.column,
        }
    }

    /// Creates a runtime error
    pub fn runtime_error(message: impl Into<String>, cause: Option<Box<VanuaError>>) -> Self {
        VanuaError::RuntimeError {
            message: message.into(),
            cause,
        }
    }

    /// Creates a generic error
    pub fn generic_error(message: impl Into<String>) -> Self {
        VanuaError::GenericError {
            message: message.into(),
        }
    }

    /// Returns whether this is a compile-time error
    pub fn is_compile_time(&self) -> bool {
        matches!(
            self,
            VanuaError::LexError { .. }
                | VanuaError::ParseError { .. }
                | VanuaError::TypeError { .. }
                | VanuaError::CodegenError { .. }
                | VanuaError::UndefinedVariable { .. }
                | VanuaError::UndefinedFunction { .. }
                | VanuaError::UndefinedClass { .. }
                | VanuaError::PatternMatchError { .. }
        )
    }

    /// Returns whether this is a runtime error
    pub fn is_runtime(&self) -> bool {
        matches!(
            self,
            VanuaError::RuntimeError { .. }
                | VanuaError::PatternMatchError { .. }
                | VanuaError::PartialApplicationError { .. }
                | VanuaError::CurryingError { .. }
                | VanuaError::CompositionError { .. }
                | VanuaError::LazyEvaluationError { .. }
                | VanuaError::InvalidOperation { .. }
        )
    }
}
