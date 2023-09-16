use crate::token::{TokenEnum, Token};
use crate::node::{prelude::*, Node};
use crate::node::function_expression::{FunctionParameter, FunctionReturnType};
use crate::errors::{ErrorTracker, Error, ErrorKind};
use crate::{GetSpan, Span};


fn is_parenthesized(expr: &Node) -> bool {
    match expr {
        Node::ParenthesizedExpression(_) => true,
        Node::UnaryExpression(UnaryExpression {expression, ..}) => is_parenthesized(expression),
        _ => false
    }
}

type StopTokenCheck<'a> = &'a dyn Fn(TokenEnum) -> bool;

pub struct Parser {
    tokens : Vec<Token>,
    next_token: usize
}

impl Parser {

    pub fn new(tokens : Vec<Token>) -> Self {
        Self {
            tokens,
            next_token: 0,
        }
    }

    fn next(&mut self) -> Token {
        if self.next_token > self.tokens.len() - 1 {
            return self.tokens[self.tokens.len() -1].clone();
        }
        let token = self.tokens[self.next_token].clone();
        self.next_token += 1;
        token
    }

    fn peek_next(&self, next: usize) -> Token {
        if self.next_token + next > self.tokens.len() - 1 {
            return self.tokens[self.tokens.len() - 1].clone();
        }
        self.tokens[self.next_token + next].clone()
    }

    fn seperator_back_until(&mut self, check_fn: fn(&Token) -> bool) {
        while !check_fn(&self.tokens[self.next_token]) && self.tokens[self.next_token].kind == TokenEnum::Separator {
            self.next_token -= 1;
        }
    }

    fn parse_function_call(&mut self, token : Token, last_precedence : u8, error_tracker : &mut ErrorTracker) -> Result<Node, Error> {
        let mut arguments = vec![];
        // Return function call without arguments when argument list ends immediately.
        let current_token = self.peek_next(0);
        if let TokenEnum::ClosingParentheses = current_token.kind {
            self.next_token += 1;
            return Ok(Node::CallExpression(CallExpression { name: token, closing_parenthesis: current_token, arguments}));
        };
        let closing_parenthesis_token = loop {
            // Parse expression until ',' or ')'
            let argument = self.parse_expression_until(last_precedence, &|t| t == TokenEnum::Comma || t == TokenEnum::ClosingParentheses, error_tracker)?;
            arguments.push(argument);
            // Function call argument list ends when seeing ')'
            let previous_token = &self.tokens[self.next_token -1];
            if let TokenEnum::ClosingParentheses = previous_token.kind {
                break previous_token.clone();
            };
        };
        Ok(Node::CallExpression(CallExpression { name: token, closing_parenthesis: closing_parenthesis_token, arguments}))
    }

    fn parse_sub_expression(&mut self, last_precedence : u8, stop_token_check : &Option<StopTokenCheck>, error_tracker : &mut ErrorTracker) -> Result<Node, Error> {
        let mut token = self.next();
        // Skip separator
        while token.kind == TokenEnum::Separator {
            token = self.next();
        }

        //println!("parse sub expr : {:?}", token);
        // TODO: Don't use this many else if lets
        return if token.kind.is_literal() {
            Ok(Node::LiteralExpression(token))
        } else if let TokenEnum::Identifier(_) = token.kind {
            // Check for function call
            let peeked_next = self.peek_next(0);

            match peeked_next.kind {
                TokenEnum::OpeningParentheses => {
                    //println!("Function call detected");
                    self.next_token += 1;
                    return Ok(self.parse_function_call(token, last_precedence, error_tracker)?);
                },
                TokenEnum::OpeningBracket => {
                    //println!("Indexing detected");
                    self.next_token += 1;
                    let index_value_expr = self.parse_expression_until_line_end(&Some(&|t| t == TokenEnum::ClosingBracket), error_tracker)?;
                    return Ok(Node::IndexExpression(IndexExpression {
                        opening_bracket: peeked_next,
                        index_value: Box::new(index_value_expr),
                        index_into: Box::new(Node::IdentifierExpression(token)),
                        closing_bracket: self.tokens[self.next_token - 1].clone(),
                    }));
                }
                _ => {}
            }
            Ok(Node::IdentifierExpression(token))
        } else if let TokenEnum::OpeningParentheses = token.kind {
            // Parse expression until ')'
            let expression = self.parse_expression_until(0, &|t| t == TokenEnum::ClosingParentheses, error_tracker)?;
            Ok(Node::ParenthesizedExpression(Box::new(expression)))
        } else if token.kind.is_unary_operator_token() {
            let expression = self.parse_sub_expression(last_precedence, stop_token_check, error_tracker)?;
            Ok(Node::UnaryExpression(UnaryExpression { operation: token, expression: Box::new(expression) }))
        } else if let TokenEnum::OpeningBrace = token.kind {
            // Skip separator if any
            let peeked_next = self.peek_next(0);
            if peeked_next.kind == TokenEnum::Separator || peeked_next.kind == TokenEnum::ClosingBrace {
                self.next_token += 1;
            }
            // Parse true branch until "}"
            let expression_list = self.parse_expression_list_until(token, &Some(|t| t == TokenEnum::ClosingBrace), error_tracker);
            Ok(expression_list)
        } else if let TokenEnum::IfKeyword = token.kind {
            // Parse condition
            let condition_expression = self.parse_expression_until_line_end(&Some(&|t| t == TokenEnum::OpeningBrace), error_tracker)?;
            // Make "{" the token that will be acquired when calling next in parse_sub_expression
            self.seperator_back_until(|t| t.kind == TokenEnum::OpeningBrace);
            // Parse true branch
            let true_branch_expression = self.parse_sub_expression(last_precedence, stop_token_check, error_tracker)?;
            // Check for else keyword
            let mut false_branch = None;
            let peeked_next = self.peek_next(0);
            if peeked_next.kind == TokenEnum::ElseKeyword {
                self.next_token += 1; // Skip else keyword
                false_branch = Some(Box::new(self.parse_sub_expression(last_precedence, stop_token_check, error_tracker)?)); // Parse false branch
            }
            Ok(Node::IfExpression(IfExpression { keyword: token, condition: Box::new(condition_expression), true_branch: Box::new(true_branch_expression), false_branch }))
        } else if let TokenEnum::FunctionKeyword = token.kind {
            // TODO: Move into own function
            // Get function name
            let function_name_identifier_token = self.next();
            if let TokenEnum::Identifier(_) = function_name_identifier_token.kind {} else {
                return Err(Error::from_span(
                    function_name_identifier_token.span,
                    format!("The function keyword is always followed by an identifier, but got: {:?}", function_name_identifier_token),
                    ErrorKind::ParsingError)
                );
            }
            // Check for "("
            if self.next().kind != TokenEnum::OpeningParentheses {
                return Err(Error::from_span(
                    self.tokens[self.next_token - 1].span.clone(),
                    format!("Expected Opening Parentheses"),
                    ErrorKind::ParsingError
                ));
            }
            // Parse argument list until ")"
            let mut parameters = vec![];
            let closing_parenthesis_token = loop {
                let new_token = self.next();
                match new_token.kind {
                    TokenEnum::ClosingParentheses => break new_token,
                    TokenEnum::Identifier(..) => {
                        let colon_token = self.next();
                        if colon_token.kind != TokenEnum::DoublePoint {
                            return Err(Error::from_span(
                                colon_token.span,
                                format!("Expected Type annotation after parameter name, but got: {:?}", colon_token),
                                ErrorKind::ParsingError
                            ))
                        }

                        let parameter_token = self.next();
                        if let TokenEnum::Identifier(_) = parameter_token.kind {} else {
                            return Err(Error::from_span(
                                colon_token.span,
                                format!("Expected type after parameter name, but got: {:?}", colon_token),
                                ErrorKind::ParsingError
                            ))
                        }

                        parameters.push(FunctionParameter {
                            name: new_token,
                            colon: colon_token,
                            parameter_type: parameter_token,
                        })
                    },
                    TokenEnum::Comma => continue,
                    _ => return Err(Error::from_span(new_token.span,format!("Unexpected token in function parameter list : {:?}", new_token), ErrorKind::ParsingError))
                }
            };

            let possible_arrow_token = self.peek_next(0);
            let mut return_type = None;
            if TokenEnum::Arrow == possible_arrow_token.kind {
                self.next_token += 1;
                let return_type_token = self.next();

                if let TokenEnum::Identifier(_) = return_type_token.kind {} else { //
                    return Err(Error::from_span(
                        return_type_token.span,
                        format!("Expected return type after arrow, but got : {:?}", return_type_token),
                        ErrorKind::ParsingError
                    ));
                }

                return_type = Some(FunctionReturnType {
                    arrow: possible_arrow_token,
                    return_type: return_type_token,
                });
            }

            // Parse function body
            let body = self.parse_sub_expression(last_precedence, stop_token_check, error_tracker)?;
            // Return function expression
            Ok(Node::FunctionExpression(FunctionExpression {
                keyword: token,
                name: function_name_identifier_token,
                parameters,
                closing_parenthesis: closing_parenthesis_token,
                body: Box::new(body),
                return_type
            }))
        } else if let TokenEnum::WhileKeyword = token.kind {
            // Parse condition
            let condition_expression = self.parse_expression_until_line_end(&Some(&|t| t == TokenEnum::OpeningBrace), error_tracker)?;
            let is_ok_condition = match &condition_expression {
                Node::FunctionExpression(_) => false,
                Node::ReturnExpression(_) => false,
                Node::BreakExpression(_) => false,
                Node::CommentExpression(_) => false,
                _ => true
            };
            if !is_ok_condition {
                return Err(Error::from_span(
                    condition_expression.get_span(),
                    format!("Unexpected condition for while loop: {:?}", condition_expression),
                    ErrorKind::ParsingError
                ));
            }
            // Make "{" the token that will be acquired when calling next in parse_sub_expression
            self.seperator_back_until(|t| t.kind == TokenEnum::OpeningBrace);
            // Parse while body
            let body_expression = self.parse_sub_expression(last_precedence, stop_token_check, error_tracker)?;
            Ok(Node::WhileExpression(WhileExpression { keyword: token, condition: Box::new(condition_expression), body: Box::new(body_expression) }))
        } else if let TokenEnum::ReturnKeyword = token.kind {
            // TODO: Make providing a return value optional
            let return_value_expression = self.parse_expression_until_line_end(stop_token_check, error_tracker)?;
            self.next_token -= 1;  // Important to make self.peek_next(0) return a stop token so there is no unexpected operator token error
            Ok(Node::ReturnExpression(ReturnExpression { keyword: token, expression: Box::new(return_value_expression) } ))
        } else if let TokenEnum::BreakKeyword = token.kind {
            Ok(Node::BreakExpression(BreakExpression { keyword: token }))
        } else if let TokenEnum::Comment(_) = token.kind {
            Ok(Node::CommentExpression(CommentExpression {
                comment: token,
                on: None
            }))
        } else {
            Err(Error::from_span(token.span,format!("Unexpected sub expression token : {:?}", token), ErrorKind::ParsingError))
        }
    }

    fn is_stop_token(&self, token_kind : TokenEnum, stop_token_check : &Option<StopTokenCheck>) -> bool {
        if let Some(function) = stop_token_check {
            if function(token_kind.clone()) {
                return true;
            }
        }

        match token_kind {
            TokenEnum::Separator => true,
            TokenEnum::EndOfFile => true,
            TokenEnum::ClosingParentheses => true,
            _ => false
        }
    }

    /// Tries to parse a node with a sub expression.
    /// if successful returns the parsed node and werther or there was a early exit due to operator precedences (Then the returned node is equal to the sub expression)
    fn parse_expression_with_sub_expression(&mut self, sub_expression : Node, last_precedence : u8, ignore_precedences: bool, stop_token_check : &Option<StopTokenCheck>, error_tracker : &mut ErrorTracker) -> Result<(Node, bool), Error> {
        // Get operator
        let operator = self.peek_next(0);

        if self.is_stop_token(operator.kind.clone(), stop_token_check) {
            self.next_token += 1;
            return Ok((sub_expression, false));
        }

        if let TokenEnum::Comment(_) = operator.kind {
            self.next_token += 1;
            return Ok((Node::CommentExpression(CommentExpression {
                comment: operator,
                on: Some(Box::new(sub_expression)),
            }), false));
        }

        // Handle assignment expressions
        let is_assignment = match operator.kind {
            TokenEnum::Equals => true,
            TokenEnum::OperatorEquals(_) => true,
            _ => false
        };
        if is_assignment {
            self.next_token += 1;
            if let Node::IdentifierExpression(_) = sub_expression.clone() {
            } else if let Node::IndexExpression(_) = sub_expression.clone() {
            } else {
                return Err(Error::from_span(operator.span,format!("Unexpected sub expression for assignment : {:?}", sub_expression), ErrorKind::ParsingError));
            }
            // Parse expression to assign to
            let mut assign_expression = self.parse_expression_until_line_end(stop_token_check, error_tracker)?; // Parse whole line since assignment is always the last step
            // Apply operator if needed
            let mut is_operator_equals = false;
            if let TokenEnum::OperatorEquals(assignment_operator) = operator.kind.clone() {
                is_operator_equals = true;
                assign_expression = Node::BinaryExpression(BinaryExpression {
                    left: Box::new(sub_expression.clone()),
                    operation : Token {
                        kind: *assignment_operator,
                        span: operator.span,
                    },
                    right: Box::new(assign_expression),
                })
            }
            return Ok((Node::AssignmentExpression(AssignmentExpression {
                to: Box::new(sub_expression),
                equals: operator,
                value: Box::new(assign_expression),
                is_operator_equals
            }), false));
        }

        // Check if operator is binary operator and create binary expression with sub expression and recursive parse expression.
        return if operator.kind.is_binary_operator_token() {
            // Check if already parsed sub expression is parenthesized so it can be ignored in the precedence checks below.
            let parenthesized = is_parenthesized(&sub_expression);

            // Stop recursion if last precedence is higher then current, but ignore parenthesized expressions.
            let current_precedence = operator.kind.get_operator_precedence().expect(&format!("Not a operator token : {:?}", operator));
            if current_precedence < last_precedence || parenthesized && !ignore_precedences { return Ok((sub_expression, true)) }
            self.next_token += 1; // Only accept peeked char if precedence is right.

            let (right, right_did_early_exit) = self.parse_expression(current_precedence, stop_token_check, error_tracker)?;

            Ok((Node::BinaryExpression(BinaryExpression {
                left: Box::new(sub_expression),
                operation: operator,
                right: Box::new(right)
            }), right_did_early_exit))
        } else {
            Err(Error::from_span(
                operator.span,
                format!("Unexpected operator token : {:?}", operator),
                ErrorKind::ParsingError
            ))
        }
    }

    fn parse_expression(&mut self, last_precedence : u8, stop_token_check : &Option<StopTokenCheck>, error_tracker : &mut ErrorTracker) -> Result<(Node, bool), Error> {
        let sub_expression = self.parse_sub_expression(last_precedence, stop_token_check, error_tracker)?;
        self.parse_expression_with_sub_expression(sub_expression, last_precedence, false, stop_token_check, error_tracker)
    }


    /// Parses expressions until reaching a stop token
    /// Returns error on encountering end of file
    /// Returns empty expression list when not parsing anything
    fn parse_expression_until(&mut self, last_precedence : u8, stop_token_check : StopTokenCheck, error_tracker : &mut ErrorTracker) -> Result<Node, Error> {
        let current_token = self.peek_next(0);
        if current_token.kind == TokenEnum::EndOfFile {
            return Err(Error::from_span(
                current_token.span,
                "Unexpected end of file".to_string(),
                ErrorKind::ParsingError
            ))
        }
        if stop_token_check(current_token.kind) {
            self.next_token += 1;
            // Return empty expression list on immediate end
            return Ok(Node::ExpressionList(ExpressionList {
                opening: Token { kind: TokenEnum::NoToken, span: Default::default() },
                expressions: vec![],
                closing: Token { kind: TokenEnum::NoToken, span: Default::default() },
            }));
        };

        // Parse expression until stop (and call parse_expression_with_sub_expression to handle precedences correctly)
        let (mut expression, mut did_early_exit) = self.parse_expression(last_precedence, &Some(stop_token_check), error_tracker)?;
        loop {
            let token_kind = &self.tokens[self.next_token - 1].kind;
            if token_kind == &TokenEnum::EndOfFile {
                return Err(Error::from_span(
                    current_token.span,
                    "Unexpected end of file".to_string(),
                    ErrorKind::ParsingError
                ))
            }
            if !did_early_exit && stop_token_check(token_kind.clone()) {
                break;
            }
            (expression, did_early_exit) = self.parse_expression_with_sub_expression(expression, last_precedence, did_early_exit, &Some(stop_token_check.clone()), error_tracker)?;
        }
        Ok(expression)
    }

    /// Calls parse expression in loop until a stop token is drawn.
    /// Calling the normal parse_expression just once will not parse the full line, when there are precedence issues.
    /// That's why this method exists.
    pub fn parse_expression_until_line_end(&mut self, stop_token_check : &Option<StopTokenCheck>, error_tracker : &mut ErrorTracker) -> Result<Node, Error> {
        let (mut result, mut did_early_exit) = self.parse_expression(0, stop_token_check, error_tracker)?;
        loop {
            let mut is_end_token = false;
            if stop_token_check.is_some() {
                is_end_token = stop_token_check.as_ref().unwrap()(self.tokens[self.next_token -1].kind.clone());
            }
            is_end_token = match self.tokens[self.next_token -1].kind {
                TokenEnum::Separator => true,
                TokenEnum::EndOfFile => true,
                _ => is_end_token
            };
            if !is_end_token {
                (result, did_early_exit) = self.parse_expression_with_sub_expression(result.clone(), 0, did_early_exit, stop_token_check, error_tracker)?;
            } else { break; }
        }
        Ok(result)
    }

    /// This function parses every line until either reaching the end of the token stream or a stop token if supplied with one.
    fn parse_expression_list_until<F: Fn(TokenEnum) -> bool>(&mut self, opening_token: Token, stop_token_check : &Option<F>, error_tracker : &mut ErrorTracker) -> Node {
        // Check for stop token first to make empty expression lists possible
        if stop_token_check.is_some() {
            let start_token = self.next_token;
            while self.tokens[self.next_token -1].kind == TokenEnum::Separator {
                self.next_token += 1;
            }
            let check_token = &self.tokens[self.next_token -1];
            if stop_token_check.as_ref().unwrap()(check_token.kind.clone()) {
                return Node::ExpressionList(ExpressionList {
                    opening: opening_token,
                    expressions: vec![],
                    closing: check_token.clone(),
                });
            }
            self.next_token = start_token;
        }

        let mut expressions = vec![];
        let parsed_result = self.parse_expression_until_line_end(&stop_token_check.as_ref().map(|f| f as StopTokenCheck), error_tracker);
        match parsed_result {
            Ok(parsed) => expressions.push(parsed),
            Err(err) => error_tracker.add_error(err)
        }
        let closing = loop {
            let start_token = self.next_token;
            while self.tokens[self.next_token -1].kind == TokenEnum::Separator {
                self.next_token += 1;
            }
            let check_token = &self.tokens[self.next_token -1];
            if stop_token_check.as_ref()
                   .map(|stop_token_check| stop_token_check(check_token.kind.clone()))
                   .unwrap_or(false)
                ||
                    TokenEnum::EndOfFile == check_token.kind {
                break check_token.clone();
            }
            self.next_token = start_token;

            let check_token = &self.tokens[self.next_token -1];
            if check_token.kind == TokenEnum::EndOfFile {
                break check_token.clone();
            } else {
                let parsed_result = self.parse_expression_until_line_end(&stop_token_check.as_ref().map(|f| f as StopTokenCheck), error_tracker);
                match parsed_result {
                    Ok(parsed) => expressions.push(parsed),
                    Err(err) => error_tracker.add_error(err)
                }
            }
        };
        Node::ExpressionList(ExpressionList {
            opening: opening_token,
            expressions,
            closing,
        })
    }

    pub fn parse_all(&mut self, error_tracker : &mut ErrorTracker) -> Node {
        self.parse_expression_list_until::<fn(TokenEnum) -> bool>(
            Token  { kind: TokenEnum::NoToken, span: Span { start_char: 0, end_char: 0 } },
            &None,
            error_tracker
        )
    }
}