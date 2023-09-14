use crate::errors::{Error, ErrorKind, ErrorTracker};
use crate::Span;
use crate::token::{Token, TokenEnum};

#[derive(Debug)]
pub struct Lexer {
    chars : Vec<char>,
    next_char_index: usize,
    current_line : usize,
    pub tokens : Vec<Token>
}


impl Lexer {
    pub fn new(text: String) -> Self {
        let chars = text.chars().collect::<Vec<_>>();
        Self {
            chars,
            next_char_index: 0,
            current_line: 0,
            tokens: vec![]
        }
    }

    fn next(&mut self) -> Option<char> {
        if self.next_char_index > self.chars.len() - 1 {
            return None
        }
        let char = self.chars[self.next_char_index].clone();
        self.next_char_index += 1;
        Some(char)
    }

    fn peek_next(&self, next: usize) -> Option<char> {
        if self.next_char_index + next > self.chars.len() - 1 {
            return None
        }
        Some(self.chars[self.next_char_index + next].clone())
    }

    /// Returns the current char index
    fn current_char(&self) -> usize {
        self.next_char_index - 1
    }

    fn current_char_span(&self) -> Span {
        Span {
            start_char: self.current_char(),
            end_char: self.current_char(),
        }
    }

    fn current_span_with_length(&self, length: usize) -> Span {
        Span {
            start_char: self.next_char_index - length,
            end_char: self.current_char(),
        }
    }

    fn has_sequence(&mut self, current_char: char, keyword: &str) -> bool {
        let mut keyword_chars = keyword.chars();
        if current_char != keyword_chars.next().unwrap() { return false; }

        let mut current_char_index = 0;
        for keyword_char in keyword_chars {
            if keyword_char != self.peek_next(current_char_index).unwrap() { return false; }
            current_char_index += 1;
        };

        for _ in 0..keyword.len() {
            self.next_char_index += 1;
        }
        true
    }

    fn next_token(&mut self, mut current_char: char, error_tracker : &mut ErrorTracker) -> Token {
        if current_char == '\r' {
            return Token {
                kind: TokenEnum::NoToken,
                span: self.current_char_span()
            }
        }

        // Check for String literal
        if current_char == '"' {
            let mut string_content = String::new();
            loop {
                let result_char = self.next();
                if result_char.is_none() {
                    return Token {
                        kind: TokenEnum::NoToken,
                        span: self.current_char_span()
                    }
                }
                let char = result_char.unwrap();
                if char == '"' { break };
                string_content.push(char);
            }
            return Token {
                kind: TokenEnum::StringLiteral(string_content.clone()),
                span: Span {
                    start_char: self.next_char_index - string_content.len(),
                    end_char: self.current_char()
                }
            };
        }

        // Check for code comments (with #) and skip until next seperator
        if current_char == '#' {
            while current_char != '\n' && current_char != ';' {
                let next_char = self.next();
                if next_char.is_none() {
                    return Token {
                        kind: TokenEnum::NoToken,
                        span: self.current_char_span()
                    }
                };
                current_char = next_char.unwrap();
            }
        }

        // Check for number
        if current_char.is_numeric() {
            let mut number_string = String::new();
            let mut found_dot = false;
            number_string.push(current_char);
            loop {
                let result_peeked_char = self.peek_next(0);
                if result_peeked_char.is_none() { break };
                let peeked_char = result_peeked_char.unwrap();
                if peeked_char.is_numeric() || peeked_char == '.' {
                    if peeked_char == '.' && !found_dot {
                        found_dot = true
                    } else if peeked_char == '.' && found_dot {
                        error_tracker.add_error(Error {
                            text: "Found float literal with more than one dot".to_string(),
                            start_char: self.next_char_index,
                            end_char: self.next_char_index,
                            kind: ErrorKind::LexingError
                        });
                        return Token {
                            kind: TokenEnum::NoToken,
                            span: Span {
                                start_char: 0,
                                end_char: 0
                            }
                        };
                    }
                    number_string.push(peeked_char);
                    self.next_char_index += 1;
                } else { break };
            }
            let token_kind = match found_dot {
                true => TokenEnum::FloatLiteral(number_string.parse::<f32>().unwrap()),
                false => TokenEnum::Number(number_string.parse::<u32>().unwrap())
            };
            return Token {
                kind: token_kind,
                span: self.current_span_with_length(number_string.len())
            };
        }

        // Check for identifier
        if current_char.is_alphabetic() {
            let mut identifier_string = String::new();
            identifier_string.push(current_char);
            loop {
                let result_peeked_char = self.peek_next(0);
                if result_peeked_char.is_none() { break };
                let peeked_char = result_peeked_char.unwrap();
                if peeked_char.is_alphabetic() || peeked_char == '_' {
                    identifier_string.push(peeked_char);
                    self.next_char_index += 1;
                } else { break };
            }

            // Check for Keywords
            let token_kind = match identifier_string.as_str() {
                "fun" => TokenEnum::FunctionKeyword,
                "return" => TokenEnum::ReturnKeyword,
                "break" => TokenEnum::BreakKeyword,
                "while" => TokenEnum::WhileKeyword,
                "if" => TokenEnum::IfKeyword,
                "else" => TokenEnum::ElseKeyword,
                // Boolean and none literal keywords
                "true" => TokenEnum::BooleanLiteral(true),
                "false" => TokenEnum::BooleanLiteral(false),
                // Alphabetic boolean operation keywords
                "and" => TokenEnum::And,
                "or" => TokenEnum::Or,
                "not" => TokenEnum::Not,
                _ => TokenEnum::Identifier(identifier_string.replace("_", "-"))
            };

            return Token {
                kind: token_kind,
                span: self.current_span_with_length(identifier_string.len())
            };
        }

        if self.has_sequence(current_char, "==") { return Token { kind: TokenEnum::DoubleEquals, span: self.current_span_with_length(2) } };
        if self.has_sequence(current_char, "!=") { return Token { kind: TokenEnum::NotEquals, span: self.current_span_with_length(2) } };
        if self.has_sequence(current_char, "->") { return Token { kind: TokenEnum::Arrow, span: self.current_span_with_length(2) } };

        // Single
        let token_kind = match current_char {
            '+' => TokenEnum::Plus,
            '-' => TokenEnum::Minus,
            '*' => TokenEnum::Multiply,
            '/' => TokenEnum::Divide,
            '>' => TokenEnum::GreaterThan,
            '<' => TokenEnum::LessThan,
            '=' => TokenEnum::Equals,
            '\t' => TokenEnum::NoToken,
            ' ' => TokenEnum::NoToken,
            '\n' => {
                self.current_line += 1;
                TokenEnum::Separator
            },
            ';' => TokenEnum::Separator,
            '.' => TokenEnum::Dot,
            ',' => TokenEnum::Comma,
            ':' => TokenEnum::DoublePoint,
            '{' => TokenEnum::OpeningBrace,
            '}' => TokenEnum::ClosingBrace,
            '(' => TokenEnum::OpeningParentheses,
            ')' => TokenEnum::ClosingParentheses,
            '[' => TokenEnum::OpeningBracket,
            ']' => TokenEnum::ClosingBracket,
            _ => {
                error_tracker.add_error(Error::from_span(
                    self.current_char_span(),
                    format!("Invalid char : '{}'", current_char),
                    ErrorKind::LexingError
                ));
                TokenEnum::NoToken
            }
        };
        let token = Token {
            kind: token_kind,
            span: self.current_char_span()
        };

        // Convert to OperatorEqual if token is an assignable operator
        if token.kind.is_assignable_operator() && self.peek_next(0).unwrap() == '=' {
            self.next();
            return Token {
                kind: TokenEnum::OperatorEquals(Box::new(token.kind.clone())),
                span: Span { start_char: token.span.start_char, end_char: self.current_char() },
            };
        }

        token
    }

    pub fn lex(&mut self, error_tracker : &mut ErrorTracker) {
        loop {
            let char = self.next();
            if char.is_none() {
                self.tokens.push(Token {
                    kind: TokenEnum::EndOfFile,
                    span: Span {
                        start_char: self.next_char_index,
                        end_char: self.next_char_index
                    }
                });
                return;
            }
            let token = self.next_token(char.unwrap(), error_tracker);
            if let TokenEnum::NoToken = token.kind {
            } else {
                self.tokens.push(token);
            }
        }
    }
}