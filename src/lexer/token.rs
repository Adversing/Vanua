use crate::error::Span;
use logos::Logos;
use std::fmt;

/// Token structure that includes type, literal value, and location information
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, span: Span) -> Self {
        Self {
            token_type,
            lexeme,
            span,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} '{}' at {}",
            self.token_type, self.lexeme, self.span
        )
    }
}

/// All possible token types in the Vanua language
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Keywords
    #[token("fun")]
    Fun,

    #[token("val")]
    Val,

    #[token("var")]
    Var,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("while")]
    While,

    #[token("for")]
    For,

    #[token("in")]
    In,

    #[token("return")]
    Return,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("class")]
    Class,

    #[token("trait")]
    Trait,

    #[token("impl")]
    Impl,

    #[token("mut")]
    Mut,

    #[token("pub")]
    Pub,

    #[token("priv")]
    Priv,

    #[token("open")]
    Open,

    #[token("override")]
    Override,

    #[token("final")]
    Final,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("null")]
    Null,

    #[token("import")]
    Import,

    #[token("package")]
    Package,

    #[token("when")]
    When,

    #[token("const")]
    Const,

    #[token("defer")]
    Defer,

    #[token("struct")]
    Struct,

    #[token("super")]
    Super,

    #[token("extends")]
    Extends,

    #[token("implements")]
    Implements,

    // Lambda calculus keywords
    #[token("curry")]
    Curry,

    #[token("lazy")]
    Lazy,

    #[token("match")]
    Match,

    #[token("case")]
    Case,

    #[token("_")]
    Underscore,

    #[token("is")]
    Is,

    #[token("as")]
    As,

    #[token("compose")]
    Compose,

    // Identifiers and literals
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    // Integer literals - Basic, Hex, Binary, and with underscores as separators
    #[regex("[0-9][0-9_]*")]
    IntLiteral,

    #[regex("0x[0-9a-fA-F][0-9a-fA-F_]*")]
    HexLiteral,

    #[regex("0b[01][01_]*")]
    BinaryLiteral,

    // Float literals with optional exponent and separators
    #[regex("[0-9][0-9_]*\\.[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?")]
    FloatLiteral,

    #[regex("[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*")]
    ScientificLiteral,

    #[regex("\"([^\"\\\\]|\\\\[\"\\\\nrtbf0])*\"")]
    StringLiteral,

    #[regex("\"\"\"([^\"]*|\"[^\"]|\"\"[^\"])*\"\"\"")]
    MultiLineString,

    // String interpolation tokens
    InterpolatedStringStart,  // String part before first #{
    InterpolatedStringMiddle, // String part between } and #{
    InterpolatedStringEnd,    // String part after last }
    InterpolationStart,       // #{
    InterpolationEnd,         // }

    #[regex("'([^'\\\\]|\\\\[\"'\\\\nrtbf0])'")]
    CharLiteral,
    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("%")]
    Percent,

    #[token("=")]
    Equal,

    #[token("==")]
    EqualEqual,

    #[token("!=")]
    BangEqual,

    #[token("<")]
    Less,

    #[token("<=")]
    LessEqual,

    #[token(">")]
    Greater,

    #[token(">=")]
    GreaterEqual,

    #[token("!")]
    Bang,

    #[token("&&")]
    AndAnd,

    #[token("||")]
    OrOr,

    #[token("::")]
    ColonColon,

    #[token("->", priority = 2)]
    Arrow,

    #[token("=>")]
    FatArrow,

    #[token("+=")]
    PlusEqual,

    #[token("-=")]
    MinusEqual,

    #[token("*=")]
    StarEqual,

    #[token("/=")]
    SlashEqual,

    #[token("%=")]
    PercentEqual,

    // Increment/Decrement operators
    #[token("++")]
    PlusPlus,

    #[token("--")]
    MinusMinus,

    #[token("&")]
    Ampersand,

    #[token("|")]
    Pipe,

    #[token("^")]
    Caret,

    #[token("~")]
    Tilde,

    // Bit shift operators
    #[token("<<")]
    LessLess,

    #[token(">>")]
    GreaterGreater,

    #[token("?.")]
    QuestionDot,

    #[token("??")]
    QuestionQuestion,

    // Lambda calculus operators
    #[token("○")]
    ComposeSymbol,

    #[token("$")]
    ApplyOperator,

    #[token("@")]
    At,

    // Delimiters
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("[")]
    LeftBracket,

    #[token("]")]
    RightBracket,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token(";")]
    Semicolon,

    #[token(":")]
    Colon,

    #[token("?")]
    Question,

    #[regex(r"[ \t\n\r]+")]
    Whitespace,

    #[regex(r"//[^\n]*")]
    Comment,

    #[regex(r"/\*([^*]|\*[^/])*\*/")]
    MultiLineComment,

    Eof,

    Error,
    #[token("new")]
    New,

    #[token("delete")]
    Delete,

    #[token("unsafe")]
    Unsafe,

    #[token("sizeof")]
    SizeOf,

    #[token("->", priority = 1)]
    ArrowPointer,

    #[token("async")]
    Async,

    #[token("await")]
    Await,

    #[token("sync")]
    Sync,

    #[token("future")]
    Future,
}
