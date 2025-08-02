use crate::ast::{
    BinaryOp, ClassDeclaration, ConstructorDeclaration, Declaration, Expression,
    FunctionDeclaration, ImplDeclaration, Import, InterpolationPart, Literal, MethodModifier,
    Package, Parameter, PrimitiveType, Program, Statement, StructDeclaration, TraitDeclaration,
    Type, TypeParameter, UnaryOp, VarDeclaration, Visibility,
};
use crate::error::{Span, VanuaError};
use crate::lexer::{Token, TokenType};

/// Context where parsing is currently happening
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParseContext {
    /// Top-level declarations (functions, classes, etc.)
    TopLevel,
    /// Inside a class body (methods, fields, constructors)
    ClassBody,
    /// Inside a trait body (method signatures)
    #[allow(dead_code)]
    TraitBody,
    /// Inside a struct body (fields)
    #[allow(dead_code)]
    StructBody,
    /// Inside an impl block (method implementations)
    #[allow(dead_code)]
    ImplBody,
    /// Inside a function body (statements, local variables)
    FunctionBody,
    /// Inside a method body (statements, local variables)
    MethodBody,
    /// Inside a constructor body (statements, local variables)
    ConstructorBody,
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, VanuaError> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
    had_error: bool,
    context_stack: Vec<ParseContext>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            had_error: false,
            context_stack: vec![ParseContext::TopLevel],
        }
    }

    // context management methods
    fn push_context(&mut self, context: ParseContext) {
        self.context_stack.push(context);
    }

    fn pop_context(&mut self) {
        if self.context_stack.len() > 1 {
            self.context_stack.pop();
        }
    }

    fn current_context(&self) -> ParseContext {
        *self.context_stack.last().unwrap_or(&ParseContext::TopLevel)
    }

    fn is_visibility_allowed(&self) -> bool {
        match self.current_context() {
            ParseContext::TopLevel
            | ParseContext::ClassBody
            | ParseContext::TraitBody
            | ParseContext::StructBody => true,
            ParseContext::ImplBody
            | ParseContext::FunctionBody
            | ParseContext::MethodBody
            | ParseContext::ConstructorBody => false,
        }
    }

    // helper function to calculate span safely
    fn calculate_span(&self, start: Span, end: Span) -> Span {
        let length = if end.line > start.line {
            // to avoid overflow saturating operations are mandatory
            let line_diff = end.line.saturating_sub(start.line);
            let line_contrib = line_diff.saturating_mul(100);
            let col_diff = (end.column.saturating_add(end.length)).saturating_sub(start.column);
            line_contrib.saturating_add(col_diff)
        } else if end.line == start.line && end.column >= start.column {
            (end.column.saturating_add(end.length)).saturating_sub(start.column)
        } else {
            // minimum length in case of error
            1
        };

        Span {
            line: start.line,
            column: start.column,
            length,
        }
    }

    fn parse_program(&mut self) -> Result<Program, VanuaError> {
        let start_span = self.peek_token_span()?;

        // optional package declaration. I might enforce this in the future.
        let mut package_decl = None;
        if self.match_token(TokenType::Package) {
            package_decl = Some(self.parse_package()?);
        }

        // import statements
        let mut imports = Vec::new();
        while self.match_token(TokenType::Import) {
            imports.push(self.parse_import()?);
        }

        // automatically import 'io' if not already imported
        let has_io_import = imports
            .iter()
            .any(|import| import.path.len() == 1 && import.path[0] == "io");

        if !has_io_import {
            let mut path = Vec::new();
            path.push("io".to_string());
            imports.push(Import::new(path, None, Span::default()));
        }

        let mut declarations = Vec::new();
        let mut first_error = None;

        while !self.is_at_end() {
            match self.parse_declaration() {
                Ok(decl) => declarations.push(decl),
                Err(err) => {
                    // store the first error encountered
                    if first_error.is_none() {
                        first_error = Some(err.clone());
                    }

                    // critical validation error check. If it is, propagate it immediately
                    if let VanuaError::ParseError { message, .. } = &err {
                        if message.contains("Method name 'new' is reserved") {
                            return Err(err);
                        }
                    }

                    // for other errors, continue with error recovery
                    if self.had_error {
                        self.synchronize();
                    } else {
                        break;
                    }
                }
            }
        }

        let end_span = if let Some(last_decl) = declarations.last() {
            Some(last_decl.span())
        } else if !imports.is_empty() {
            Some(imports.last().unwrap().span)
        } else if let Some(pkg) = &package_decl {
            Some(pkg.span)
        } else {
            None
        };

        let span = if let Some(end) = end_span {
            self.calculate_span(start_span, end)
        } else {
            start_span
        };

        if let Some(error) = first_error {
            return Err(error);
        }

        Ok(Program::new(package_decl, imports, declarations, span))
    }

    fn parse_package(&mut self) -> Result<Package, VanuaError> {
        let name = self.consume_identifier("Expected package name")?;
        let span = self.previous_token_span().unwrap();

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after package declaration",
        )?;

        Ok(Package::new(name, span))
    }

    fn parse_import(&mut self) -> Result<Import, VanuaError> {
        let start_span = self.previous_token_span().unwrap();

        // parse the import path (dot-separated identifiers)
        let mut path = Vec::new();
        path.push(self.consume_identifier("Expected import path")?);

        while self.match_token(TokenType::Dot) {
            path.push(self.consume_identifier("Expected identifier after '.'")?);
        }

        // parse optional alias
        let alias = if self.match_token(TokenType::As) {
            Some(self.consume_identifier("Expected alias after 'as'")?)
        } else {
            None
        };

        let end_span = self.previous_token_span().unwrap();
        let span = self.calculate_span(start_span, end_span);

        self.consume(TokenType::Semicolon, "Expected ';' after import")?;

        Ok(Import::new(path, alias, span))
    }

    fn parse_visibility(&mut self) -> Result<Visibility, VanuaError> {
        if self.check(TokenType::Pub) {
            if !self.is_visibility_allowed() {
                let span = self.peek_token_span()?;
                let context_name = match self.current_context() {
                    ParseContext::FunctionBody => "function body",
                    ParseContext::MethodBody => "method body",
                    ParseContext::ConstructorBody => "constructor body",
                    ParseContext::ImplBody => "impl block",
                    _ => "this context",
                };
                return Err(VanuaError::ParseError {
                    line: span.line,
                    column: span.column,
                    message: format!(
                        "Visibility modifier 'pub' is not allowed in {}",
                        context_name
                    ),
                });
            }
            self.advance(); // consume 'pub'
            Ok(Visibility::Public)
        } else {
            // private by default policy
            Ok(Visibility::Private)
        }
    }

    fn parse_method_modifier(&mut self) -> Result<MethodModifier, VanuaError> {
        if self.match_token(TokenType::Open) {
            Ok(MethodModifier::Open)
        } else if self.match_token(TokenType::Override) {
            Ok(MethodModifier::Override)
        } else if self.match_token(TokenType::Final) {
            Ok(MethodModifier::Final)
        } else {
            Ok(MethodModifier::None) // default to no modifier
        }
    }

    fn parse_declaration(&mut self) -> Result<Declaration, VanuaError> {
        let visibility = self.parse_visibility()?;

        if self.check(TokenType::Async) || self.check(TokenType::Fun) {
            self.parse_function_declaration_with_visibility(visibility)
        } else if self.match_token(TokenType::Class) {
            self.parse_class_declaration_with_visibility(visibility)
        } else if self.match_token(TokenType::Trait) {
            self.parse_trait_declaration_with_visibility(visibility)
        } else if self.match_token(TokenType::Struct) {
            self.parse_struct_declaration_with_visibility(visibility)
        } else if self.match_token(TokenType::Impl) {
            self.parse_impl_declaration()
        } else {
            Err(self.error("Expected declaration"))
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.iter().nth(self.current);
        if token.is_some() {
            self.current += 1;
        }
        token
    }

    fn peek(&self) -> Option<Token> {
        if self.current < self.tokens.len() {
            Some(self.tokens[self.current].clone())
        } else {
            None
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().map_or(true, |t| t.token_type == TokenType::Eof)
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if let Some(token) = self.peek() {
            if token.token_type == token_type {
                self.advance();
                return true;
            }
        }
        false
    }

    fn match_any(&mut self, types: &[TokenType]) -> bool {
        for &token_type in types {
            if self.match_token(token_type) {
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.peek().map_or(false, |t| t.token_type == token_type)
    }

    fn check_identifier(&self, expected: &str) -> bool {
        self.peek().map_or(false, |t| {
            t.token_type == TokenType::Identifier && t.lexeme == expected
        })
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<(), VanuaError> {
        if self.check(token_type) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(message))
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<String, VanuaError> {
        if self.check(TokenType::Identifier) {
            self.advance();
            let identifier = self.tokens[self.current - 1].lexeme.clone();
            Ok(identifier)
        } else {
            Err(self.error(message))
        }
    }

    fn previous_token_span(&self) -> Option<Span> {
        if self.current > 0 {
            Some(self.tokens[self.current - 1].span)
        } else {
            None
        }
    }

    fn peek_token_span(&mut self) -> Result<Span, VanuaError> {
        if let Some(token) = self.peek() {
            Ok(token.span)
        } else {
            Err(self.error("Unexpected end of input"))
        }
    }

    fn error(&mut self, message: &str) -> VanuaError {
        self.had_error = true;

        if let Some(token) = self.peek() {
            VanuaError::ParseError {
                line: token.span.line,
                column: token.span.column,
                message: message.to_string(),
            }
        } else {
            if let Some(last) = self.tokens.last() {
                VanuaError::ParseError {
                    line: last.span.line,
                    column: last.span.column + last.span.length,
                    message: message.to_string(),
                }
            } else {
                // how did I get here??
                VanuaError::ParseError {
                    line: 0,
                    column: 0,
                    message: message.to_string(),
                }
            }
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.tokens[self.current - 1].token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().unwrap().token_type {
                TokenType::Fun
                | TokenType::Class
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Return
                | TokenType::Import => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn parse_function_declaration_with_visibility(
        &mut self,
        visibility: Visibility,
    ) -> Result<Declaration, VanuaError> {
        let span_start = self.peek_token_span()?;
        let method_modifier = self.parse_method_modifier()?;
        let is_async = self.match_token(TokenType::Async);

        self.consume(TokenType::Fun, "Expected 'fun' keyword")?;

        let name = self.consume_identifier("Expected function name")?;

        let mut type_params = Vec::new();
        if self.match_token(TokenType::Less) {
            loop {
                let type_param_name = self.consume_identifier("Expected type parameter name")?;
                let span = self.previous_token_span().unwrap_or(Span::default());
                type_params.push(TypeParameter::new(type_param_name, Vec::new(), span));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }

            self.consume(TokenType::Greater, "Expected '>' after type parameters")?;
        }

        // parameters
        self.consume(TokenType::LeftParen, "Expected '(' after function name")?;
        let params = self.parse_parameters()?;

        let return_type = if self.match_token(TokenType::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        // function body
        let body = if self.match_token(TokenType::LeftBrace) {
            self.push_context(ParseContext::FunctionBody);
            let block = self.parse_block_statement()?;
            self.pop_context();
            Some(block)
        } else {
            self.consume(
                TokenType::Semicolon,
                "Expected ';' after function signature",
            )?;
            None
        };

        let span_end = match &body {
            Some(stmt) => stmt.span(),
            None => self.previous_token_span().unwrap_or(span_start),
        };

        let span = self.calculate_span(span_start, span_end);

        Ok(Declaration::Function(FunctionDeclaration {
            name,
            params,
            return_type,
            body,
            type_params,
            visibility,
            is_async,
            method_modifier,
            span,
        }))
    }

    fn parse_class_declaration_with_visibility(
        &mut self,
        visibility: Visibility,
    ) -> Result<Declaration, VanuaError> {
        let span_start = self.peek_token_span()?;

        // class name
        let name = self.consume_identifier("Expected class name")?;

        // type parameters
        let mut type_params = Vec::new();
        if self.match_token(TokenType::Less) {
            // parse type parameters
            loop {
                let type_param_name = self.consume_identifier("Expected type parameter name")?;
                let span = self.previous_token_span().unwrap_or(Span::default());
                type_params.push(TypeParameter::new(type_param_name, Vec::new(), span));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }

            self.consume(TokenType::Greater, "Expected '>' after type parameters")?;
        }

        let mut superclasses = Vec::new();
        if self.match_token(TokenType::Extends) {
            loop {
                superclasses.push(self.parse_type()?);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        let mut implements = Vec::new();
        if self.match_token(TokenType::Implements) {
            loop {
                implements.push(self.parse_type()?);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        // class body
        self.consume(TokenType::LeftBrace, "Expected '{' before class body")?;

        // enter class body context
        self.push_context(ParseContext::ClassBody);

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut constructors = Vec::new();

        while !self.match_token(TokenType::RightBrace) {
            let member_visibility = self.parse_visibility()?;
            let method_modifier = self.parse_method_modifier()?;

            if method_modifier == MethodModifier::Open && member_visibility == Visibility::Public {
                let span = self.peek_token_span()?;
                return Err(VanuaError::ParseError {
                    line: span.line,
                    column: span.column,
                    message: "Methods marked 'open' are public by default and cannot be explicitly marked 'pub'".to_string(),
                });
            }

            if method_modifier == MethodModifier::Open && member_visibility == Visibility::Private {
                let span = self.peek_token_span()?;
                return Err(VanuaError::ParseError {
                    line: span.line,
                    column: span.column,
                    message: "Methods marked 'open' cannot be private".to_string(),
                });
            }

            if self.match_token(TokenType::Fun) {
                let mut method = self.parse_method()?;

                method.visibility = if method_modifier == MethodModifier::Open {
                    Visibility::Public
                } else {
                    member_visibility
                };
                method.method_modifier = method_modifier;
                methods.push(method);
            } else if self.check_identifier("constructor") {
                self.advance(); // consume "constructor"
                let mut constructor = self.parse_constructor_params_and_body(member_visibility)?;
                constructor.visibility = member_visibility;
                constructors.push(constructor);
            } else if self.match_token(TokenType::Var) {
                let mut field = self.parse_field()?;
                field.visibility = member_visibility;
                fields.push(field);
            } else {
                return Err(VanuaError::ParseError {
                    line: self.peek_token_span()?.line,
                    column: self.peek_token_span()?.column,
                    message: "Expected 'fun', 'constructor', or 'var' in class body".to_string(),
                });
            }
        }

        self.pop_context();

        let span_end = self.previous_token_span().unwrap_or(Span::default());
        let span = self.calculate_span(span_start, span_end);

        Ok(Declaration::Class(ClassDeclaration {
            name,
            superclasses,
            implements,
            type_params,
            methods,
            fields,
            visibility,
            constructors,
            span,
        }))
    }

    fn parse_constructor_params_and_body(
        &mut self,
        visibility: Visibility,
    ) -> Result<ConstructorDeclaration, VanuaError> {
        let span_start = self.peek_token_span()?;

        self.consume(TokenType::LeftParen, "Expected '(' after 'constructor'")?;

        let mut params = Vec::new();
        if !self.match_token(TokenType::RightParen) {
            loop {
                let is_mutable = self.match_token(TokenType::Mut);
                let param_name = self.consume_identifier("Expected parameter name")?;

                let mut param_type = None;
                if self.match_token(TokenType::Colon) {
                    param_type = Some(self.parse_type()?);
                }

                let mut default_value = None;
                if self.match_token(TokenType::Equal) {
                    default_value = Some(self.parse_expression()?);
                }

                let span = self.previous_token_span().unwrap_or(Span::default());
                params.push(Parameter {
                    name: param_name,
                    param_type,
                    default_value,
                    is_mutable,
                    span,
                });

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }

            self.consume(TokenType::RightParen, "Expected ')' after parameters")?;
        }

        // check for invalid return type specification
        if self.check(TokenType::Colon) {
            let span = self.peek_token_span()?;
            return Err(VanuaError::ParseError {
                line: span.line,
                column: span.column,
                message:
                    "Constructors cannot have return types. Remove the return type specification."
                        .to_string(),
            });
        }

        // body
        self.consume(TokenType::LeftBrace, "Expected '{' before constructor body")?;
        self.push_context(ParseContext::ConstructorBody);
        let statements = self.parse_block()?;
        self.pop_context();
        let body_span = self.previous_token_span().unwrap_or(Span::default());
        let body = Statement::Block(statements, body_span);

        let span_end = self.previous_token_span().unwrap_or(Span::default());
        let span = self.calculate_span(span_start, span_end);

        Ok(ConstructorDeclaration {
            params,
            body,
            visibility,
            span,
        })
    }

    fn parse_method(&mut self) -> Result<FunctionDeclaration, VanuaError> {
        let span_start = self.peek_token_span()?;

        let is_async = self.match_token(TokenType::Async);

        if self.check(TokenType::New) {
            return Err(VanuaError::ParseError {
                line: self.peek_token_span().unwrap_or(Span::default()).line,
                column: self.peek_token_span().unwrap_or(Span::default()).column,
                message: "Method name 'new' is reserved and auto-generated by the Vanua Compiler"
                    .to_string(),
            });
        }

        let name = self.consume_identifier("Expected method name")?;

        let mut type_params = Vec::new();
        if self.match_token(TokenType::Less) {
            loop {
                let type_param_name = self.consume_identifier("Expected type parameter name")?;
                let span = self.previous_token_span().unwrap_or(Span::default());
                type_params.push(TypeParameter::new(type_param_name, Vec::new(), span));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }

            self.consume(TokenType::Greater, "Expected '>' after type parameters")?;
        }

        self.consume(TokenType::LeftParen, "Expected '(' after method name")?;
        let params = self.parse_parameters()?;

        let return_type = if self.match_token(TokenType::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = if self.match_token(TokenType::LeftBrace) {
            self.push_context(ParseContext::MethodBody);
            let block = self.parse_block_statement()?;
            self.pop_context();
            Some(block)
        } else {
            self.consume(TokenType::Semicolon, "Expected ';' after method signature")?;
            None
        };

        let span_end = match &body {
            Some(stmt) => stmt.span(),
            None => self.previous_token_span().unwrap_or(span_start),
        };

        let span = self.calculate_span(span_start, span_end);

        Ok(FunctionDeclaration {
            name,
            params,
            return_type,
            body,
            type_params,
            visibility: Visibility::Private, // overridden by caller
            is_async,
            method_modifier: MethodModifier::None, // overridden by caller
            span,
        })
    }

    fn parse_field(&mut self) -> Result<VarDeclaration, VanuaError> {
        let span_start = self.peek_token_span()?;
        let is_mutable = self.match_token(TokenType::Mut); // TODO remove this
        let name = self.consume_identifier("Expected field name")?;

        // Field type
        let mut var_type = None;
        if self.match_token(TokenType::Colon) {
            var_type = Some(self.parse_type()?);
        }

        // Initializer
        let mut initializer = None;
        if self.match_token(TokenType::Equal) {
            initializer = Some(self.parse_expression()?);
        }

        self.consume(TokenType::Semicolon, "Expected ';' after field declaration")?;

        let span_end = self.previous_token_span().unwrap_or(Span::default());
        let span = self.calculate_span(span_start, span_end);

        Ok(VarDeclaration {
            name,
            var_type,
            initializer,
            is_mutable,
            visibility: Visibility::Private, // overridden by caller
            span,
        })
    }

    fn parse_trait_declaration_with_visibility(
        &mut self,
        visibility: Visibility,
    ) -> Result<Declaration, VanuaError> {
        let span_start = self.peek_token_span()?;
        let name = self.consume_identifier("Expected trait name")?;

        let mut type_params = Vec::new();
        if self.match_token(TokenType::Less) {
            loop {
                let type_param_name = self.consume_identifier("Expected type parameter name")?;
                let span = self.previous_token_span().unwrap_or(Span::default());
                type_params.push(TypeParameter::new(type_param_name, Vec::new(), span));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }

            self.consume(TokenType::Greater, "Expected '>' after type parameters")?;
        }

        let mut extends = Vec::new();
        if self.match_token(TokenType::Extends) {
            loop {
                extends.push(self.parse_type()?);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::LeftBrace, "Expected '{' before trait body")?;

        let mut methods = Vec::new();
        let mut required_methods = Vec::new();

        while !self.match_token(TokenType::RightBrace) {
            if self.match_token(TokenType::Fun) {
                let method = self.parse_method()?;

                if method.body.is_some() {
                    methods.push(method);
                } else {
                    required_methods.push(method);
                }
            } else {
                return Err(self.error("Expected method declaration in trait"));
            }
        }

        let span_end = self.previous_token_span().unwrap_or(Span::default());
        let span = self.calculate_span(span_start, span_end);

        Ok(Declaration::Trait(TraitDeclaration {
            name,
            extends,
            type_params,
            methods,
            required_methods,
            visibility,
            span,
        }))
    }

    fn parse_struct_declaration_with_visibility(
        &mut self,
        visibility: Visibility,
    ) -> Result<Declaration, VanuaError> {
        let span_start = self.peek_token_span()?;
        let name = self.consume_identifier("Expected struct name")?;

        let mut type_params = Vec::new();
        if self.match_token(TokenType::Less) {
            loop {
                let type_param_name = self.consume_identifier("Expected type parameter name")?;
                let span = self.previous_token_span().unwrap_or(Span::default());
                type_params.push(TypeParameter::new(type_param_name, Vec::new(), span));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }

            self.consume(TokenType::Greater, "Expected '>' after type parameters")?;
        }

        self.consume(TokenType::LeftBrace, "Expected '{' before struct body")?;

        let mut fields = Vec::new();

        while !self.match_token(TokenType::RightBrace) {
            let field_visibility = self.parse_visibility()?;
            let mut field = self.parse_field()?;
            field.visibility = field_visibility;
            fields.push(field);
        }

        let span_end = self.previous_token_span().unwrap_or(Span::default());
        let span = self.calculate_span(span_start, span_end);

        Ok(Declaration::Struct(StructDeclaration {
            name,
            type_params,
            fields,
            visibility,
            span,
        }))
    }

    fn parse_impl_declaration(&mut self) -> Result<Declaration, VanuaError> {
        let span_start = self.peek_token_span()?;

        let mut trait_type = None;

        if !self
            .peek()
            .map_or(false, |token| token.token_type == TokenType::For)
        {
            trait_type = Some(self.parse_type()?);

            self.consume(TokenType::For, "Expected 'for' after trait name in impl")?;
        }

        let for_type = self.parse_type()?;

        let mut type_params = Vec::new();
        if self.match_token(TokenType::Less) {
            loop {
                let type_param_name = self.consume_identifier("Expected type parameter name")?;
                let span = self.previous_token_span().unwrap_or(Span::default());
                type_params.push(TypeParameter::new(type_param_name, Vec::new(), span));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }

            self.consume(TokenType::Greater, "Expected '>' after type parameters")?;
        }

        self.consume(TokenType::LeftBrace, "Expected '{' before impl body")?;
        let mut methods = Vec::new();

        while !self.match_token(TokenType::RightBrace) {
            if self.match_token(TokenType::Fun) {
                let method = self.parse_method()?;
                methods.push(method);
            } else {
                return Err(self.error("Expected method declaration in impl block"));
            }
        }

        let span_end = self.previous_token_span().unwrap_or(Span::default());
        let span = self.calculate_span(span_start, span_end);

        Ok(Declaration::Impl(ImplDeclaration {
            trait_type,
            for_type,
            type_params,
            methods,
            span,
        }))
    }

    fn parse_block(&mut self) -> Result<Vec<Statement>, VanuaError> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }

        self.consume(TokenType::RightBrace, "Expected '}' after block")?;

        Ok(statements)
    }

    fn parse_type(&mut self) -> Result<Type, VanuaError> {
        let span = self.peek_token_span()?;

        // ptr (*T)
        if self.match_token(TokenType::Star) {
            let target_type = self.parse_type()?;
            return Ok(Type::Pointer(Box::new(target_type), span));
        }

        // ref (&T)
        if self.match_token(TokenType::Ampersand) {
            let target_type = self.parse_type()?;
            return Ok(Type::Reference(Box::new(target_type), span));
        }

        // primitive types
        if self.match_token(TokenType::Identifier) {
            let name = self.tokens[self.current - 1].lexeme.clone();

            let primitive_type = match name.as_str() {
                "Int" => Some(PrimitiveType::Int),
                "Float" => Some(PrimitiveType::Float),
                "Bool" => Some(PrimitiveType::Bool),
                "Char" => Some(PrimitiveType::Char),
                "String" => Some(PrimitiveType::String),
                "Nothing" => Some(PrimitiveType::Nothing),
                _ => None,
            };

            if let Some(prim_type) = primitive_type {
                return Ok(Type::Primitive(prim_type, span));
            }

            if name == "Unit" || name == "Void" {
                // TODO: move Void into its own type
                return Ok(Type::Unit(span));
            }

            let mut type_args = Vec::new();

            if self.match_token(TokenType::Less) {
                loop {
                    type_args.push(self.parse_type()?);

                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                }

                self.consume(TokenType::Greater, "Expected '>' after type arguments")?;
            }

            Ok(Type::Named(name, type_args, span))
        } else if self.match_token(TokenType::LeftBracket) {
            let element_type = self.parse_type()?;
            self.consume(
                TokenType::RightBracket,
                "Expected ']' after array element type",
            )?;

            Ok(Type::Array(Box::new(element_type), span))
        } else if self.match_token(TokenType::LeftParen) {
            let mut param_types = Vec::new();

            if !self.match_token(TokenType::RightParen) {
                loop {
                    param_types.push(self.parse_type()?);

                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                }

                self.consume(
                    TokenType::RightParen,
                    "Expected ')' after function parameter types",
                )?;
            }

            self.consume(TokenType::Arrow, "Expected '->' in function type")?;
            let return_type = self.parse_type()?;

            Ok(Type::Function(param_types, Box::new(return_type), span))
        } else {
            Err(self.error("Expected type"))
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, VanuaError> {
        if self.check(TokenType::Pub) {
            let span = self.peek_token_span()?;
            let context_name = match self.current_context() {
                ParseContext::FunctionBody => "function body",
                ParseContext::MethodBody => "method body",
                ParseContext::ConstructorBody => "constructor body",
                _ => "this context",
            };
            return Err(VanuaError::ParseError {
                line: span.line,
                column: span.column,
                message: format!("Visibility modifier 'pub' is not allowed in {}. Use 'pub' only on top-level declarations, class members, or struct fields.", context_name),
            });
        }

        if self.match_token(TokenType::Var) || self.match_token(TokenType::Val) {
            self.parse_var_declaration()
        } else if self.match_token(TokenType::If) {
            self.parse_if_statement()
        } else if self.match_token(TokenType::While) {
            self.parse_while_statement()
        } else if self.match_token(TokenType::For) {
            self.parse_for_statement()
        } else if self.match_token(TokenType::Return) {
            self.parse_return_statement()
        } else if self.match_token(TokenType::Break) {
            let span = self.previous_token_span().unwrap_or(Span::default());
            self.consume(TokenType::Semicolon, "Expected ';' after break statement")?;
            Ok(Statement::Break(span))
        } else if self.match_token(TokenType::Continue) {
            let span = self.previous_token_span().unwrap_or(Span::default());
            self.consume(
                TokenType::Semicolon,
                "Expected ';' after continue statement",
            )?;
            Ok(Statement::Continue(span))
        } else if self.match_token(TokenType::Defer) {
            self.parse_defer_statement()
        } else if self.match_token(TokenType::Unsafe) {
            self.parse_unsafe_block()
        } else if self.peek().map_or(false, |t| t.lexeme == "alloc") {
            self.advance(); // consume "alloc"
            self.parse_alloc_statement()
        } else if self.peek().map_or(false, |t| t.lexeme == "free") {
            self.advance(); // consume "free"
            self.parse_free_statement()
        } else if self.match_token(TokenType::LeftBrace) {
            self.parse_block_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_if_statement(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());

        self.consume(TokenType::LeftParen, "Expected '(' after 'if'")?;
        let condition = self.parse_expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after if condition")?;

        let then_branch = self.parse_statement()?;

        let else_branch = if self.match_token(TokenType::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let end_span = match &else_branch {
            Some(stmt) => stmt.span(),
            None => then_branch.span(),
        };

        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::If(
            condition,
            Box::new(then_branch),
            else_branch,
            span,
        ))
    }

    fn parse_while_statement(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());

        self.consume(TokenType::LeftParen, "Expected '(' after 'while'")?;
        let condition = self.parse_expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after while condition")?;

        let body = self.parse_statement()?;
        let end_span = body.span();
        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::While(condition, Box::new(body), span))
    }

    fn parse_for_statement(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());

        if self.check(TokenType::Identifier) {
            let current_pos = self.current;
            self.advance(); // consume identifier
            if self.match_token(TokenType::In) {
                self.current = current_pos; // reset position
                return self.parse_for_in_statement(start_span);
            }
            self.current = current_pos; // reset position if not for-in
        }

        self.consume(TokenType::LeftParen, "Expected '(' after 'for'")?;

        let initializer = if self.match_token(TokenType::Var) {
            Some(self.parse_var_declaration()?)
        } else if self.check(TokenType::Identifier) {
            Some(self.parse_expression_statement()?)
        } else {
            return Err(
                self.error("For loops require either 'var' declaration or existing variable")
            );
        };

        let (var_name, var_type) = match &initializer {
            Some(Statement::VarDecl(var_decl, _)) => {
                (var_decl.name.clone(), var_decl.var_type.clone())
            }
            Some(Statement::Expression(Expression::Variable(name, _), _)) => (name.clone(), None),
            Some(Statement::Expression(Expression::Assign(target, _, _), _)) => {
                if let Expression::Variable(name, _) = &**target {
                    (name.clone(), None)
                } else {
                    return Err(self.error("For loop assignment target must be a variable"));
                }
            }
            _ => {
                return Err(self.error(
                    "For loop initializer must be a variable declaration or variable reference",
                ))
            }
        };

        let start_expr = match &initializer {
            Some(Statement::VarDecl(var_decl, _)) => match &var_decl.initializer {
                Some(expr) => expr.clone(),
                None => return Err(self.error("For loop variable must be initialized")),
            },
            Some(Statement::Expression(Expression::Variable(_, span), _)) => {
                Expression::Variable(var_name.clone(), *span)
            }
            Some(Statement::Expression(Expression::Assign(_, expr, _), _)) => (**expr).clone(),
            _ => return Err(self.error("Invalid for loop initializer")),
        };

        let end_expr = if self.check(TokenType::Semicolon) {
            let token = self.peek_token_span()?;
            Expression::Literal(Literal::Int(10), token) // TODO: refactor default value mechanism
        } else {
            let condition = self.parse_expression()?;
            match condition {
                Expression::Binary(_, BinaryOp::Lt, right, _) => *right,
                Expression::Binary(_, BinaryOp::Lte, right, _) => *right,
                Expression::Binary(_, BinaryOp::Gt, left, _) => *left,
                Expression::Binary(_, BinaryOp::Gte, left, _) => *left,
                _ => condition,
            }
        };
        self.consume(TokenType::Semicolon, "Expected ';' after loop condition")?;

        let step_option = if self.check(TokenType::RightParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume(TokenType::RightParen, "Expected ')' after for clauses")?;

        let body = self.parse_statement()?;
        let end_span = body.span();
        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::For(
            var_name,
            var_type,
            start_expr,
            end_expr,
            step_option,
            Box::new(body),
            span,
        ))
    }

    fn parse_for_in_statement(&mut self, start_span: Span) -> Result<Statement, VanuaError> {
        let var_name = self.consume_identifier("Expected variable name in for-in loop")?;

        self.consume(
            TokenType::In,
            "Expected 'in' after variable name in for-in loop",
        )?;

        self.consume(
            TokenType::LeftParen,
            "Expected '(' after 'in' in for-in loop",
        )?;
        let collection = self.parse_expression()?;
        self.consume(
            TokenType::RightParen,
            "Expected ')' after collection in for-in loop",
        )?;

        let body = self.parse_statement()?;
        let end_span = body.span();
        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::ForIn(var_name, collection, Box::new(body), span))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());

        if self.current_context() == ParseContext::ConstructorBody {
            return Err(VanuaError::ParseError {
                line: start_span.line,
                column: start_span.column,
                message: "Return statements are not allowed in constructors. Constructors automatically return the created instance.".to_string(),
            });
        }

        let value = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };

        self.consume(TokenType::Semicolon, "Expected ';' after return value")?;
        let end_span = self.previous_token_span().unwrap_or(start_span);
        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::Return(value, span))
    }

    fn parse_defer_statement(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());

        let statement = self.parse_statement()?;
        let end_span = statement.span();
        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::Defer(Box::new(statement), span))
    }

    fn parse_block_statement(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());

        let mut statements = Vec::new();
        let mut first_error = None;

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    if first_error.is_none() {
                        first_error = Some(err);
                    }

                    self.had_error = true;
                    self.synchronize();

                    if self.check(TokenType::RightBrace) || self.is_at_end() {
                        break;
                    }
                }
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' after block")?;
        let end_span = self.previous_token_span().unwrap_or(start_span);
        let span = self.calculate_span(start_span, end_span);

        if let Some(error) = first_error {
            return Err(error);
        }

        Ok(Statement::Block(statements, span))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, VanuaError> {
        let expr = self.parse_expression()?;
        let start_span = expr.span();

        self.consume(TokenType::Semicolon, "Expected ';' after expression")?;
        let end_span = self.previous_token_span().unwrap_or(start_span);
        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::Expression(expr, span))
    }

    fn parse_var_declaration(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());

        let is_mutable = match self.tokens[self.current - 1].token_type {
            TokenType::Var => true,
            TokenType::Val => false,
            _ => {
                return Err(self.error("Expected 'var' or 'val' for variable declaration"));
            }
        };

        let name = self.consume_identifier("Expected variable name")?;
        let name_span = self.previous_token_span().unwrap_or(Span::default());

        let type_annotation = if self.match_token(TokenType::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let initializer = if self.match_token(TokenType::Equal) {
            Some(self.parse_expression()?)
        } else {
            if !is_mutable {
                return Err(
                    self.error("Immutable variable declaration (val) requires an initializer")
                );
            }
            if type_annotation.is_none() {
                return Err(self.error("Variable declaration without type annotation requires an initializer for type inference"));
            }
            None
        };

        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration",
        )?;
        let end_span = self.previous_token_span().unwrap_or(start_span);
        let span = self.calculate_span(start_span, end_span);

        let var_decl = VarDeclaration {
            name: name.clone(),
            var_type: type_annotation,
            initializer,
            is_mutable,
            visibility: Visibility::Private,
            span: name_span,
        };

        Ok(Statement::VarDecl(var_decl, span))
    }

    fn parse_expression(&mut self) -> Result<Expression, VanuaError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expression, VanuaError> {
        let expr = self.parse_ternary()?;

        if self.match_token(TokenType::Equal) {
            let _equals_span = self.previous_token_span().unwrap_or(Span::default());
            let value = self.parse_assignment()?;

            return match expr {
                Expression::Variable(_, _) | Expression::Property(_, _, _) => {
                    let span = self.calculate_span(expr.span(), value.span());
                    Ok(Expression::Assign(Box::new(expr), Box::new(value), span))
                }
                _ => Err(self.error("Invalid assignment target")),
            };
        } else if self.match_any(&[
            TokenType::PlusEqual,
            TokenType::MinusEqual,
            TokenType::StarEqual,
            TokenType::SlashEqual,
            TokenType::PercentEqual,
        ]) {
            let _operator_span = self.previous_token_span().unwrap_or(Span::default());
            let token_type = self.tokens[self.current - 1].token_type;
            let binary_op = match token_type {
                TokenType::PlusEqual => BinaryOp::Add,
                TokenType::MinusEqual => BinaryOp::Sub,
                TokenType::StarEqual => BinaryOp::Mul,
                TokenType::SlashEqual => BinaryOp::Div,
                TokenType::PercentEqual => BinaryOp::Mod,
                _ => unreachable!(),
            };

            let value = self.parse_assignment()?;

            return match expr {
                Expression::Variable(_, _) | Expression::Property(_, _, _) => {
                    let span = self.calculate_span(expr.span(), value.span());
                    Ok(Expression::CompoundAssign(
                        Box::new(expr),
                        binary_op,
                        Box::new(value),
                        span,
                    ))
                }
                _ => Err(self.error("Invalid assignment target")),
            };
        }

        Ok(expr)
    }

    fn parse_ternary(&mut self) -> Result<Expression, VanuaError> {
        let expr = self.parse_null_coalesce()?;

        // TODO: Implement proper ternary operator parsing that doesn't interfere with nullability check

        Ok(expr)
    }

    fn parse_null_coalesce(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_logical_or()?;

        while self.match_token(TokenType::QuestionQuestion) {
            let span = self.previous_token_span().unwrap_or(Span::default());
            let right = self.parse_logical_or()?;
            let end_span = right.span();
            let total_span = self.calculate_span(span, end_span);
            expr = Expression::Binary(
                Box::new(expr),
                BinaryOp::NullCoalesce,
                Box::new(right),
                total_span,
            );
        }

        Ok(expr)
    }

    fn parse_logical_or(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_logical_and()?;

        while self.match_token(TokenType::OrOr) {
            let span = self.previous_token_span().unwrap_or(Span::default());
            let right = self.parse_logical_and()?;
            let end_span = right.span();
            let total_span = self.calculate_span(span, end_span);
            expr = Expression::Binary(Box::new(expr), BinaryOp::Or, Box::new(right), total_span);
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_bitwise_or()?;

        while self.match_token(TokenType::AndAnd) {
            let span = self.previous_token_span().unwrap_or(Span::default());
            let right = self.parse_bitwise_or()?;
            let end_span = right.span();
            let total_span = self.calculate_span(span, end_span);
            expr = Expression::Binary(Box::new(expr), BinaryOp::And, Box::new(right), total_span);
        }

        Ok(expr)
    }

    fn parse_bitwise_or(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_bitwise_xor()?;

        while self.match_token(TokenType::Pipe) {
            let span = self.previous_token_span().unwrap_or(Span::default());
            let right = self.parse_bitwise_xor()?;
            let end_span = right.span();
            let total_span = self.calculate_span(span, end_span);
            expr = Expression::Binary(Box::new(expr), BinaryOp::BitOr, Box::new(right), total_span);
        }

        Ok(expr)
    }

    fn parse_bitwise_xor(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_bitwise_and()?;

        while self.match_token(TokenType::Caret) {
            let span = self.previous_token_span().unwrap_or(Span::default());
            let right = self.parse_bitwise_and()?;
            let end_span = right.span();
            let total_span = self.calculate_span(span, end_span);
            expr = Expression::Binary(
                Box::new(expr),
                BinaryOp::BitXor,
                Box::new(right),
                total_span,
            );
        }

        Ok(expr)
    }

    fn parse_bitwise_and(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_equality()?;

        while self.match_token(TokenType::Ampersand) {
            let span = self.previous_token_span().unwrap_or(Span::default());
            let right = self.parse_equality()?;
            let end_span = right.span();
            let total_span = self.calculate_span(span, end_span);
            expr = Expression::Binary(
                Box::new(expr),
                BinaryOp::BitAnd,
                Box::new(right),
                total_span,
            );
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_comparison()?;

        while self.match_any(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let operator_span = self.previous_token_span().unwrap_or(Span::default());
            let token_type = self.tokens[self.current - 1].token_type;
            let operator = match token_type {
                TokenType::EqualEqual => BinaryOp::Eq,
                TokenType::BangEqual => BinaryOp::NotEq,
                _ => unreachable!(),
            };

            let right = self.parse_comparison()?;
            let end_span = right.span();
            let total_span = self.calculate_span(operator_span, end_span);
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right), total_span);
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_term()?;

        while self.match_any(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator_span = self.previous_token_span().unwrap_or(Span::default());
            let token_type = self.tokens[self.current - 1].token_type;
            let operator = match token_type {
                TokenType::Greater => BinaryOp::Gt,
                TokenType::GreaterEqual => BinaryOp::Gte,
                TokenType::Less => BinaryOp::Lt,
                TokenType::LessEqual => BinaryOp::Lte,
                _ => unreachable!(),
            };

            let right = self.parse_term()?;
            let end_span = right.span();
            let total_span = self.calculate_span(operator_span, end_span);
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right), total_span);
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_factor()?;

        while self.match_any(&[TokenType::Plus, TokenType::Minus]) {
            let operator_span = self.previous_token_span().unwrap_or(Span::default());
            let token_type = self.tokens[self.current - 1].token_type;
            let operator = match token_type {
                TokenType::Plus => BinaryOp::Add,
                TokenType::Minus => BinaryOp::Sub,
                _ => unreachable!(),
            };

            let right = self.parse_factor()?;
            let end_span = right.span();
            let total_span = self.calculate_span(operator_span, end_span);
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right), total_span);
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_shift()?;

        while self.match_any(&[TokenType::Star, TokenType::Slash, TokenType::Percent]) {
            let operator_span = self.previous_token_span().unwrap_or(Span::default());
            let token_type = self.tokens[self.current - 1].token_type;
            let operator = match token_type {
                TokenType::Star => BinaryOp::Mul,
                TokenType::Slash => BinaryOp::Div,
                TokenType::Percent => BinaryOp::Mod,
                _ => unreachable!(),
            };

            let right = self.parse_shift()?;
            let end_span = right.span();
            let total_span = self.calculate_span(operator_span, end_span);
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right), total_span);
        }

        Ok(expr)
    }

    fn parse_shift(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_unary()?;

        while self.match_any(&[TokenType::LessLess, TokenType::GreaterGreater]) {
            let operator_span = self.previous_token_span().unwrap_or(Span::default());
            let token_type = self.tokens[self.current - 1].token_type;
            let operator = match token_type {
                TokenType::LessLess => BinaryOp::BitShl,
                TokenType::GreaterGreater => BinaryOp::BitShr,
                _ => unreachable!(),
            };

            let right = self.parse_unary()?;
            let end_span = right.span();
            let total_span = self.calculate_span(operator_span, end_span);
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right), total_span);
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, VanuaError> {
        if self.match_any(&[TokenType::Bang, TokenType::Minus]) {
            let operator = match self.tokens[self.current - 1].token_type {
                TokenType::Minus => UnaryOp::Negate,
                TokenType::Bang => UnaryOp::Not,
                _ => unreachable!(),
            };

            let span_start = self.previous_token_span().unwrap_or(Span::default());
            let right = self.parse_unary()?;
            let span_end = right.span();
            let span = self.calculate_span(span_start, span_end);

            return Ok(Expression::Unary(operator, Box::new(right), span));
        } else if self.match_any(&[TokenType::PlusPlus, TokenType::MinusMinus]) {
            // prefix increment/decrement (++i, --i)
            let operator = match self.tokens[self.current - 1].token_type {
                TokenType::PlusPlus => UnaryOp::PreIncrement,
                TokenType::MinusMinus => UnaryOp::PreDecrement,
                _ => unreachable!(),
            };

            let span_start = self.previous_token_span().unwrap_or(Span::default());
            let right = self.parse_unary()?;
            let span_end = right.span();
            let span = self.calculate_span(span_start, span_end);

            // validate that the operand is a valid lvalue
            return match right {
                Expression::Variable(_, _) | Expression::Property(_, _, _) => {
                    Ok(Expression::Unary(operator, Box::new(right), span))
                }
                _ => Err(self.error("Invalid operand for increment/decrement operator")),
            };
        } else if self.match_token(TokenType::Star) {
            let span_start = self.previous_token_span().unwrap_or(Span::default());
            let expr = self.parse_unary()?; // recursive support for **ptr
            let span_end = expr.span();
            let span = self.calculate_span(span_start, span_end);

            return Ok(Expression::Dereference(Box::new(expr), span));
        } else if self.match_token(TokenType::Ampersand) {
            let span_start = self.previous_token_span().unwrap_or(Span::default());
            let expr = self.parse_unary()?;
            let span_end = expr.span();
            let span = self.calculate_span(span_start, span_end);

            return Ok(Expression::AddressOf(Box::new(expr), span));
        }

        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expression, VanuaError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.match_token(TokenType::Dot) {
                let span = self.previous_token_span().unwrap_or(Span::default());

                let name = if self.check(TokenType::Identifier) {
                    self.consume_identifier("Expected property name after '.'")?
                } else if self.match_token(TokenType::New) {
                    "new".to_string()
                } else {
                    return Err(self.error("Expected property name after '.'"));
                };
                let name_span = self.previous_token_span().unwrap_or(Span::default());
                let total_span = self.calculate_span(span, name_span);

                if self.match_token(TokenType::LeftParen) {
                    let method_name = name;
                    let mut args = Vec::new();

                    if !self.check(TokenType::RightParen) {
                        loop {
                            args.push(self.parse_expression()?);

                            if !self.match_token(TokenType::Comma) {
                                break;
                            }
                        }
                    }

                    self.consume(TokenType::RightParen, "Expected ')' after arguments")?;
                    let end_span = self.previous_token_span().unwrap_or(Span::default());
                    let call_span = self.calculate_span(span, end_span);
                    expr = Expression::Method(Box::new(expr), method_name, args, call_span);
                } else {
                    expr = Expression::Property(Box::new(expr), name, total_span);
                }
            } else if self.match_token(TokenType::QuestionDot) {
                let span = self.previous_token_span().unwrap_or(Span::default());
                let name = self.consume_identifier("Expected property name after '?.'")?;
                let name_span = self.previous_token_span().unwrap_or(Span::default());
                let total_span = self.calculate_span(span, name_span);

                if self.match_token(TokenType::LeftParen) {
                    let method_name = name;
                    let mut args = Vec::new();

                    if !self.check(TokenType::RightParen) {
                        loop {
                            args.push(self.parse_expression()?);

                            if !self.match_token(TokenType::Comma) {
                                break;
                            }
                        }
                    }

                    self.consume(TokenType::RightParen, "Expected ')' after arguments")?;
                    let end_span = self.previous_token_span().unwrap_or(Span::default());
                    let call_span = self.calculate_span(span, end_span);
                    expr = Expression::SafeMethod(Box::new(expr), method_name, args, call_span);
                } else {
                    expr = Expression::SafeProperty(Box::new(expr), name, total_span);
                }
            } else if self.match_token(TokenType::LeftBracket) {
                // array indexing (arr[index])
                let start_span = self.previous_token_span().unwrap_or(Span::default());
                let index = self.parse_expression()?;
                self.consume(TokenType::RightBracket, "Expected ']' after array index")?;
                let end_span = self.previous_token_span().unwrap_or(Span::default());
                let index_span = self.calculate_span(start_span, end_span);
                expr = Expression::Index(Box::new(expr), Box::new(index), index_span);
            } else if self.match_any(&[TokenType::PlusPlus, TokenType::MinusMinus]) {
                // postfix increment/decrement (i++, i--)
                let operator = match self.tokens[self.current - 1].token_type {
                    TokenType::PlusPlus => UnaryOp::PostIncrement,
                    TokenType::MinusMinus => UnaryOp::PostDecrement,
                    _ => unreachable!(),
                };

                let operator_span = self.previous_token_span().unwrap_or(Span::default());
                let span = self.calculate_span(expr.span(), operator_span);

                // validate that the operand is a valid lvalue
                match expr {
                    Expression::Variable(_, _) | Expression::Property(_, _, _) => {
                        expr = Expression::Unary(operator, Box::new(expr), span);
                    }
                    _ => return Err(self.error("Invalid operand for increment/decrement operator")),
                }
            } else if self.match_token(TokenType::Question) {
                // nullability check operator (value?)
                let operator_span = self.previous_token_span().unwrap_or(Span::default());
                let span = self.calculate_span(expr.span(), operator_span);

                expr = Expression::Unary(UnaryOp::NullabilityCheck, Box::new(expr), span);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression, VanuaError> {
        let span = self.previous_token_span().unwrap_or(Span::default());
        let mut args = Vec::new();

        if !self.check(TokenType::RightParen) {
            loop {
                args.push(self.parse_expression()?);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expected ')' after arguments")?;
        let end_span = self.previous_token_span().unwrap_or(Span::default());
        let call_span = self.calculate_span(span, end_span);

        Ok(Expression::Call(Box::new(callee), args, call_span))
    }

    fn parse_primary(&mut self) -> Result<Expression, VanuaError> {
        let span = self.peek_token_span()?;

        if self.match_token(TokenType::False) {
            Ok(Expression::Literal(Literal::Bool(false), span))
        } else if self.match_token(TokenType::True) {
            Ok(Expression::Literal(Literal::Bool(true), span))
        } else if self.match_token(TokenType::Null) {
            Ok(Expression::Literal(Literal::Null, span))
        } else if self.match_token(TokenType::IntLiteral) {
            // parse integer literal
            let lexeme = self.tokens[self.current - 1].lexeme.clone();
            let value = lexeme.parse::<i64>().unwrap_or(0);
            Ok(Expression::Literal(Literal::Int(value), span))
        } else if self.match_token(TokenType::FloatLiteral) {
            // parse float literal
            let lexeme = self.tokens[self.current - 1].lexeme.clone();
            let value = lexeme.parse::<f64>().unwrap_or(0.0);
            Ok(Expression::Literal(Literal::Float(value), span))
        } else if self.match_token(TokenType::StringLiteral) {
            // parse string literal
            let mut lexeme = self.tokens[self.current - 1].lexeme.clone();
            // remove quotes
            lexeme = lexeme[1..lexeme.len() - 1].to_string();
            // process escape sequences
            lexeme = self.unescape_string(&lexeme);
            Ok(Expression::Literal(Literal::String(lexeme), span))
        } else if self.check(TokenType::InterpolatedStringStart) {
            // parse interpolated string
            self.parse_interpolated_string(span)
        } else if self.match_token(TokenType::CharLiteral) {
            // parse character literal
            let lexeme = self.tokens[self.current - 1].lexeme.clone();
            // remove quotes and extract the character
            let ch = lexeme.chars().nth(1).unwrap_or('\0');
            Ok(Expression::Literal(Literal::Char(ch), span))
        } else if self.match_token(TokenType::Identifier) {
            // variable reference or struct literal
            let name = self.tokens[self.current - 1].lexeme.clone();

            if name == "this" {
                return Ok(Expression::This(span));
            }

            if self.check(TokenType::LeftBrace) {
                return self.parse_struct_literal(name, span);
            }

            Ok(Expression::Variable(name, span))
        } else if self.match_token(TokenType::New) {
            return Err(VanuaError::ParseError {
                line: span.line,
                column: span.column,
                message: "The 'new Class()' syntax is not supported. Use 'Class.new()' instead."
                    .to_string(),
            });
        } else if self.match_token(TokenType::Await) {
            // await expression
            let expr = self.parse_expression()?;
            let end_span = expr.span();
            let span = self.calculate_span(span, end_span);
            Ok(Expression::Await(Box::new(expr), span))
        } else if self.match_token(TokenType::LeftParen) {
            // grouping expression
            let expr = self.parse_expression()?;
            self.consume(TokenType::RightParen, "Expected ')' after expression")?;
            Ok(expr)
        } else if self.match_token(TokenType::Super) {
            if self.check(TokenType::LeftParen) {
                self.consume(TokenType::LeftParen, "Expected '(' after 'super'")?;
                let mut args = Vec::new();
                if !self.check(TokenType::RightParen) {
                    loop {
                        args.push(self.parse_expression()?);
                        if !self.match_token(TokenType::Comma) {
                            break;
                        }
                    }
                }
                self.consume(TokenType::RightParen, "Expected ')' after super arguments")?;

                Ok(Expression::Call(
                    Box::new(Expression::Super(String::new(), span)),
                    args,
                    span,
                ))
            } else {
                // super.method call
                self.consume(TokenType::Dot, "Expected '.' after 'super'")?;
                let method = self.consume_identifier("Expected superclass method name")?;
                Ok(Expression::Super(method, span))
            }
        } else if self.match_token(TokenType::LeftBracket) {
            let mut elements = Vec::new();

            if !self.match_token(TokenType::RightBracket) {
                loop {
                    elements.push(self.parse_expression()?);

                    if !self.match_token(TokenType::Comma) {
                        break;
                    }

                    if self.match_token(TokenType::RightBracket) {
                        break;
                    }
                }

                self.consume(TokenType::RightBracket, "Expected ']' after array elements")?;
            }

            Ok(Expression::Array(elements, span))
        } else if self.match_token(TokenType::LeftBrace) {
            // map literal
            let mut pairs = Vec::new();

            if !self.match_token(TokenType::RightBrace) {
                loop {
                    let key = self.parse_expression()?;
                    self.consume(TokenType::Colon, "Expected ':' after map key")?;
                    let value = self.parse_expression()?;

                    pairs.push((key, value));

                    if !self.match_token(TokenType::Comma) {
                        break;
                    }

                    if self.match_token(TokenType::RightBrace) {
                        break;
                    }
                }

                self.consume(TokenType::RightBrace, "Expected '}' after map pairs")?;
            }

            Ok(Expression::Map(pairs, span))
        } else {
            Err(self.error("Expected expression"))
        }
    }

    fn parse_unsafe_block(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());

        self.consume(TokenType::LeftBrace, "Expected '{' after 'unsafe'")?;

        let mut statements = Vec::new();

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            match self.parse_statement() {
                Ok(stmt) => statements.push(stmt),
                Err(_) => {
                    self.had_error = true;
                    self.synchronize();

                    if self.check(TokenType::RightBrace) || self.is_at_end() {
                        break;
                    }
                }
            }
        }

        self.consume(TokenType::RightBrace, "Expected '}' after unsafe block")?;
        let end_span = self.previous_token_span().unwrap_or(start_span);
        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::UnsafeBlock(statements, span))
    }

    fn parse_alloc_statement(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());
        let name = self.consume_identifier("Expected variable name after 'alloc'")?;

        self.consume(
            TokenType::Colon,
            "Expected ':' after variable name in alloc",
        )?;
        let var_type = self.parse_type()?;

        let size = if self.match_token(TokenType::LeftBracket) {
            let size_expr = self.parse_expression()?;
            self.consume(TokenType::RightBracket, "Expected ']' after array size")?;
            Some(size_expr)
        } else {
            None
        };

        self.consume(TokenType::Semicolon, "Expected ';' after alloc statement")?;
        let end_span = self.previous_token_span().unwrap_or(start_span);
        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::Alloc(name, var_type, size, span))
    }

    fn parse_free_statement(&mut self) -> Result<Statement, VanuaError> {
        let start_span = self.previous_token_span().unwrap_or(Span::default());
        let expr = self.parse_expression()?;

        self.consume(TokenType::Semicolon, "Expected ';' after free statement")?;
        let end_span = self.previous_token_span().unwrap_or(start_span);
        let span = self.calculate_span(start_span, end_span);

        Ok(Statement::Free(expr, span))
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, VanuaError> {
        let mut params = Vec::new();

        if !self.match_token(TokenType::RightParen) {
            loop {
                let is_mutable = self.match_token(TokenType::Mut);
                let param_name = self.consume_identifier("Expected parameter name")?;

                let mut param_type = None;
                if self.match_token(TokenType::Colon) {
                    param_type = Some(self.parse_type()?);
                }

                let mut default_value = None;
                if self.match_token(TokenType::Equal) {
                    default_value = Some(self.parse_expression()?);
                }

                let span = self.previous_token_span().unwrap_or(Span::default());
                params.push(Parameter {
                    name: param_name,
                    param_type,
                    default_value,
                    is_mutable,
                    span,
                });

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }

            self.consume(TokenType::RightParen, "Expected ')' after parameters")?;
        }

        Ok(params)
    }

    fn parse_struct_literal(
        &mut self,
        struct_name: String,
        start_span: Span,
    ) -> Result<Expression, VanuaError> {
        self.consume(TokenType::LeftBrace, "Expected '{' for struct literal")?;

        let mut fields = Vec::new();

        if !self.check(TokenType::RightBrace) {
            loop {
                let field_name =
                    self.consume_identifier("Expected field name in struct literal")?;

                self.consume(
                    TokenType::Equal,
                    "Expected '=' after field name in struct literal",
                )?;

                let field_value = self.parse_expression()?;

                fields.push((field_name, field_value));

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(
            TokenType::RightBrace,
            "Expected '}' after struct literal fields",
        )?;
        let end_span = self.previous_token_span().unwrap_or(start_span);
        let total_span = self.calculate_span(start_span, end_span);

        Ok(Expression::StructLiteral(struct_name, fields, total_span))
    }

    /// Parse an interpolated string expression
    fn parse_interpolated_string(&mut self, start_span: Span) -> Result<Expression, VanuaError> {
        let mut parts = Vec::new();
        let mut end_span = start_span;

        // first string part
        if self.match_token(TokenType::InterpolatedStringStart) {
            let mut lexeme = self.tokens[self.current - 1].lexeme.clone();
            if lexeme.starts_with('"') && lexeme.ends_with('"') {
                lexeme = lexeme[1..lexeme.len() - 1].to_string();
            }
            lexeme = self.unescape_string(&lexeme);
            if !lexeme.is_empty() {
                parts.push(InterpolationPart::String(lexeme));
            }
        }

        while self.check(TokenType::InterpolationStart) {
            self.advance();

            let expr = self.parse_expression()?;
            parts.push(InterpolationPart::Expression(expr));

            self.consume(
                TokenType::InterpolationEnd,
                "Expected '}' after interpolation expression",
            )?;
            end_span = self.previous_token_span().unwrap_or(start_span);

            if self.match_token(TokenType::InterpolatedStringMiddle) {
                let lexeme = self.tokens[self.current - 1].lexeme.clone();
                let processed = self.unescape_string(&lexeme);
                if !processed.is_empty() {
                    parts.push(InterpolationPart::String(processed));
                }
            } else if self.match_token(TokenType::InterpolatedStringEnd) {
                let lexeme = self.tokens[self.current - 1].lexeme.clone();
                let processed = self.unescape_string(&lexeme);
                if !processed.is_empty() {
                    parts.push(InterpolationPart::String(processed));
                }
                end_span = self.previous_token_span().unwrap_or(start_span);
                break;
            } else {
                return Err(VanuaError::ParseError {
                    line: end_span.line,
                    column: end_span.column,
                    message: "Expected string part after interpolation expression".to_string(),
                });
            }
        }

        let span = self.calculate_span(start_span, end_span);
        Ok(Expression::InterpolatedString(parts, span))
    }

    fn unescape_string(&self, s: &str) -> String {
        let mut result = String::new();
        let mut chars = s.chars();

        while let Some(ch) = chars.next() {
            // FIXME this is duplicated
            if ch == '\\' {
                if let Some(escaped) = chars.next() {
                    match escaped {
                        'n' => result.push('\n'),
                        'r' => result.push('\r'),
                        't' => result.push('\t'),
                        'b' => result.push('\u{0008}'), // backspace
                        'f' => result.push('\u{000C}'), // form feed
                        '0' => result.push('\0'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        '\'' => result.push('\''),
                        _ => {
                            result.push('\\');
                            result.push(escaped);
                        }
                    }
                } else {
                    result.push('\\');
                }
            } else {
                result.push(ch);
            }
        }

        result
    }
}
