pub mod token;

use crate::error::{Span, VanuaError};
use logos::Logos;

pub use self::token::{Token, TokenType};

/// Perform lexical analysis on the input source string
pub fn tokenize(source: &str) -> Result<Vec<Token>, VanuaError> {
    let source = if source.starts_with('\u{FEFF}') {
        let mut chars = source.chars();
        chars.next(); // UTF-8/BOM sequence skip
        chars.as_str()
    } else {
        source
    };

    let mut tokens = Vec::new();
    let mut chars = source.char_indices().peekable();
    let mut line = 1;
    let mut column = 1;

    while let Some((pos, ch)) = chars.next() {
        let start_line = line;
        let start_column = column;

        match ch {
            '"' => {
                let string_tokens =
                    tokenize_string(&mut chars, &mut line, &mut column, start_line, start_column)?;
                tokens.extend(string_tokens);
            }
            c if c.is_whitespace() => {
                if c == '\n' {
                    line += 1;
                    column = 1;
                } else {
                    column += 1;
                }
            }
            '/' if chars.peek().map(|(_, c)| *c) == Some('/') => {
                chars.next(); // consume second '/'
                column += 2;

                while let Some((_, c)) = chars.peek() {
                    if *c == '\n' {
                        break;
                    }
                    chars.next();
                    column += 1;
                }
            }
            '/' if chars.peek().map(|(_, c)| *c) == Some('*') => {
                chars.next(); // consume '*'
                column += 2;
                let mut depth = 1;
                while depth > 0 && chars.peek().is_some() {
                    match chars.next() {
                        Some((_, '/')) if chars.peek().map(|(_, c)| *c) == Some('*') => {
                            chars.next(); // consume '*'
                            depth += 1;
                            column += 2;
                        }
                        Some((_, '*')) if chars.peek().map(|(_, c)| *c) == Some('/') => {
                            chars.next(); // consume '/'
                            depth -= 1;
                            column += 2;
                        }
                        Some((_, '\n')) => {
                            line += 1;
                            column = 1;
                        }
                        Some((_, _)) => {
                            column += 1;
                        }
                        None => break,
                    }
                }
            }
            _ => {
                let remaining = &source[pos..];
                let mut temp_lexer = TokenType::lexer(remaining);

                if let Some(token_result) = temp_lexer.next() {
                    let token_slice = temp_lexer.slice();
                    let token_type = match token_result {
                        Ok(tt) => tt,
                        Err(_) => {
                            return Err(VanuaError::LexError {
                                line,
                                column,
                                message: format!("Invalid token at {}:{}: '{}'", line, column, ch),
                            });
                        }
                    };

                    if matches!(
                        token_type,
                        TokenType::Whitespace | TokenType::Comment | TokenType::MultiLineComment
                    ) {
                        let token_len = token_slice.len();
                        for _ in 1..token_len {
                            if let Some((_, c)) = chars.next() {
                                if c == '\n' {
                                    line += 1;
                                    column = 1;
                                } else {
                                    column += 1;
                                }
                            }
                        }
                        continue;
                    }

                    let lexeme = match token_type {
                        TokenType::IntLiteral
                        | TokenType::FloatLiteral
                        | TokenType::ScientificLiteral
                        | TokenType::HexLiteral
                        | TokenType::BinaryLiteral => token_slice.replace("_", ""),
                        _ => token_slice.to_string(),
                    };

                    let span = Span {
                        line: start_line,
                        column: start_column,
                        length: token_slice.len(),
                    };

                    tokens.push(Token::new(token_type, lexeme, span));

                    let token_len = token_slice.len();
                    for _ in 1..token_len {
                        if let Some((_, c)) = chars.next() {
                            if c == '\n' {
                                line += 1;
                                column = 1;
                            } else {
                                column += 1;
                            }
                        }
                    }
                } else {
                    return Err(VanuaError::LexError {
                        line,
                        column,
                        message: format!("Unexpected character: '{}'", ch),
                    });
                }
            }
        }
    }

    // EOF token
    tokens.push(Token::new(
        TokenType::Eof,
        "".to_string(),
        Span {
            line,
            column,
            length: 0,
        },
    ));

    Ok(tokens)
}

/// Tokenize a string literal, handling interpolation expressions
fn tokenize_string(
    chars: &mut std::iter::Peekable<std::str::CharIndices>,
    line: &mut usize,
    column: &mut usize,
    start_line: usize,
    start_column: usize,
) -> Result<Vec<Token>, VanuaError> {
    let mut tokens = Vec::new();
    let mut string_content = String::new();
    let mut has_interpolation = false;
    let mut is_first_part = true;

    *column += 1; // skip opening quote

    while let Some((_, ch)) = chars.peek().copied() {
        match ch {
            '"' => {
                chars.next(); // consume closing quote
                *column += 1;

                if has_interpolation {
                    let token_type = if is_first_part {
                        TokenType::StringLiteral
                    } else {
                        TokenType::InterpolatedStringEnd
                    };
                    tokens.push(Token::new(
                        token_type,
                        if is_first_part {
                            format!("\"{}\"", string_content)
                        } else {
                            string_content.clone()
                        },
                        Span {
                            line: start_line,
                            column: start_column,
                            length: string_content.len() + 2,
                        },
                    ));
                } else {
                    // simple string literal
                    tokens.push(Token::new(
                        TokenType::StringLiteral,
                        format!("\"{}\"", string_content),
                        Span {
                            line: start_line,
                            column: start_column,
                            length: string_content.len() + 2,
                        },
                    ));
                }
                break;
            }
            '#' => {
                // string interpolation start
                chars.next(); // consume '#'
                *column += 1;

                if chars.peek().map(|(_, c)| *c) == Some('{') {
                    chars.next(); // consume '{'
                    *column += 1;

                    has_interpolation = true;

                    // add the string part before interpolation
                    let token_type = if is_first_part {
                        TokenType::InterpolatedStringStart
                    } else {
                        TokenType::InterpolatedStringMiddle
                    };

                    tokens.push(Token::new(
                        token_type,
                        string_content.clone(),
                        Span {
                            line: *line,
                            column: *column - string_content.len() - 2,
                            length: string_content.len(),
                        },
                    ));

                    // add interpolation start token
                    tokens.push(Token::new(
                        TokenType::InterpolationStart,
                        "#{".to_string(),
                        Span {
                            line: *line,
                            column: *column - 2,
                            length: 2,
                        },
                    ));

                    // parse the interpolation expression
                    let expr_tokens = tokenize_interpolation_expression(chars, line, column)?;
                    tokens.extend(expr_tokens);

                    tokens.push(Token::new(
                        TokenType::InterpolationEnd,
                        "}".to_string(),
                        Span {
                            line: *line,
                            column: *column - 1,
                            length: 1,
                        },
                    ));

                    // reset for next part
                    string_content.clear();
                    is_first_part = false;
                } else {
                    // just a regular '#' character
                    string_content.push('#');
                }
            }
            '\\' => {
                chars.next(); // consume '\'
                *column += 1;

                if let Some((_, escaped_char)) = chars.next() {
                    *column += 1;
                    match escaped_char {
                        'n' => string_content.push('\n'),
                        't' => string_content.push('\t'),
                        'r' => string_content.push('\r'),
                        '\\' => string_content.push('\\'),
                        '"' => string_content.push('"'),
                        '0' => string_content.push('\0'),
                        'b' => string_content.push('\u{0008}'), // backspace
                        'f' => string_content.push('\u{000C}'), // form feed
                        c => {
                            return Err(VanuaError::LexError {
                                line: *line,
                                column: *column,
                                message: format!("Invalid escape sequence: \\{}", c),
                            });
                        }
                    }
                } else {
                    return Err(VanuaError::LexError {
                        line: *line,
                        column: *column,
                        message: "Unterminated escape sequence".to_string(),
                    });
                }
            }
            '\n' => {
                return Err(VanuaError::LexError {
                    line: *line,
                    column: *column,
                    message: "Unterminated string literal".to_string(),
                });
            }
            c => {
                chars.next(); // consume character
                string_content.push(c);
                if c == '\n' {
                    *line += 1;
                    *column = 1;
                } else {
                    *column += 1;
                }
            }
        }
    }

    if tokens.is_empty() {
        return Err(VanuaError::LexError {
            line: start_line,
            column: start_column,
            message: "Unterminated string literal".to_string(),
        });
    }

    Ok(tokens)
}

/// Tokenize an interpolation expression inside #{}
fn tokenize_interpolation_expression(
    chars: &mut std::iter::Peekable<std::str::CharIndices>,
    line: &mut usize,
    column: &mut usize,
) -> Result<Vec<Token>, VanuaError> {
    let mut tokens = Vec::new();
    let mut brace_depth = 1; // already inside one brace
    let mut expr_chars = Vec::new();

    while let Some((_pos, ch)) = chars.next() {
        match ch {
            '{' => {
                brace_depth += 1;
                expr_chars.push(ch);
                *column += 1;
            }
            '}' => {
                brace_depth -= 1;
                if brace_depth == 0 {
                    // found the closing brace for the interpolation
                    *column += 1;
                    break;
                } else {
                    expr_chars.push(ch);
                    *column += 1;
                }
            }
            '\n' => {
                expr_chars.push(ch);
                *line += 1;
                *column = 1;
            }
            c => {
                expr_chars.push(c);
                *column += 1;
            }
        }
    }

    if brace_depth > 0 {
        return Err(VanuaError::LexError {
            line: *line,
            column: *column,
            message: "Unterminated interpolation expression".to_string(),
        });
    }

    // convert the collected characters back to a string
    let expr_string: String = expr_chars.into_iter().collect();

    if expr_string.trim().is_empty() {
        return Err(VanuaError::LexError {
            line: *line,
            column: *column,
            message: "Empty interpolation expression".to_string(),
        });
    }

    // tokenize the expression string recursively
    let expr_tokens = tokenize(&expr_string)?;

    // filter out the EOF token from the expression tokens
    for token in expr_tokens {
        if token.token_type != TokenType::Eof {
            tokens.push(token);
        }
    }

    Ok(tokens)
}

/// Test module
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_lexing() {
        let source = "val x = 42";
        let tokens = tokenize(source).unwrap();

        assert_eq!(tokens.len(), 5); // val, x, =, 42, EOF
        assert_eq!(tokens[0].token_type, TokenType::Val);
        assert_eq!(tokens[1].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].token_type, TokenType::Equal);
        assert_eq!(tokens[3].token_type, TokenType::IntLiteral);
        assert_eq!(tokens[4].token_type, TokenType::Eof);
    }

    #[test]
    fn test_numeric_literals() {
        let source = "42 0xFF 0b1010 3.14 1.2e3 1_000_000";
        let tokens = tokenize(source).unwrap();

        assert_eq!(tokens.len(), 7); // 6 literals + EOF
        assert_eq!(tokens[0].token_type, TokenType::IntLiteral);
        assert_eq!(tokens[0].lexeme, "42");

        assert_eq!(tokens[1].token_type, TokenType::HexLiteral);
        assert_eq!(tokens[1].lexeme, "0xFF");

        assert_eq!(tokens[2].token_type, TokenType::BinaryLiteral);
        assert_eq!(tokens[2].lexeme, "0b1010");

        assert_eq!(tokens[3].token_type, TokenType::FloatLiteral);
        assert_eq!(tokens[3].lexeme, "3.14");

        assert_eq!(tokens[4].token_type, TokenType::ScientificLiteral);
        assert_eq!(tokens[4].lexeme, "1.2e3");

        assert_eq!(tokens[5].token_type, TokenType::IntLiteral);
        assert_eq!(tokens[5].lexeme, "1000000");
    }

    #[test]
    fn test_multiline_comment() {
        let source = "val x /* This is a\nmultiline comment */ = 42";
        let tokens = tokenize(source).unwrap();

        assert_eq!(tokens.len(), 5); // val, x, =, 42, EOF
        assert_eq!(tokens[0].token_type, TokenType::Val);
        assert_eq!(tokens[1].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].token_type, TokenType::Equal);
        assert_eq!(tokens[3].token_type, TokenType::IntLiteral);
    }

    #[test]
    fn test_multiline_string() {
        let source = "val x = \"\"\"This is a\nmultiline string\"\"\"";
        let tokens = tokenize(source).unwrap();

        assert_eq!(tokens.len(), 5); // val, x, =, string, EOF
        assert_eq!(tokens[0].token_type, TokenType::Val);
        assert_eq!(tokens[1].token_type, TokenType::Identifier);
        assert_eq!(tokens[2].token_type, TokenType::Equal);
        assert_eq!(tokens[3].token_type, TokenType::MultiLineString);
    }
}
