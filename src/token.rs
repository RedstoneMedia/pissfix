use std::fmt::{Debug, Formatter};
use crate::{GetSpan, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenEnum {
    Number(u32),
    FloatLiteral(f32),
    BooleanLiteral(bool),
    StringLiteral(String),
    Identifier(String),
    Comment(String),

    IfKeyword,
    WhileKeyword,
    ForKeyword,
    ElseKeyword,
    FunctionKeyword,

    InKeyword,
    ReturnKeyword,
    BreakKeyword,

    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    And,
    Or,
    Not,
    DoubleEquals,
    NotEquals,
    Range(bool),

    Equals,
    OperatorEquals(Box<TokenEnum>),

    Separator,
    Comma,
    DoublePoint,
    Arrow,
    Underscore,

    OpeningParentheses,
    ClosingParentheses,
    OpeningBrace,
    ClosingBrace,
    OpeningBracket,
    ClosingBracket,

    EndOfFile,
    NoToken
}


impl TokenEnum {

    pub fn is_unary_operator_token(&self) -> bool {
        match self {
            TokenEnum::Plus => true,
            TokenEnum::Minus => true,
            TokenEnum::Not => true,
            _ => false
        }
    }

    pub fn is_binary_operator_token(&self) -> bool {
        match self {
            TokenEnum::Plus => true,
            TokenEnum::Minus => true,
            TokenEnum::Multiply => true,
            TokenEnum::Divide => true,
            TokenEnum::GreaterThan => true,
            TokenEnum::LessThan => true,
            TokenEnum::DoubleEquals => true,
            TokenEnum::NotEquals => true,
            TokenEnum::And => true,
            TokenEnum::Or => true,
            TokenEnum::Range(_) => true,
            _ => false
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            TokenEnum::Number(_) => true,
            TokenEnum::StringLiteral(_) => true,
            TokenEnum::FloatLiteral(_) => true,
            TokenEnum::BooleanLiteral(_) => true,
            _ => false
        }
    }

    pub fn is_assignable_operator(&self) -> bool {
        match self {
            TokenEnum::Plus => true,
            TokenEnum::Minus => true,
            TokenEnum::Multiply => true,
            TokenEnum::Divide => true,
            _ => false
        }
    }

    pub fn get_operator_precedence(&self) -> Option<u8> {
        match self {
            TokenEnum::Plus => Some(3),
            TokenEnum::Minus => Some(3),
            TokenEnum::Multiply => Some(4),
            TokenEnum::Divide => Some(4),

            TokenEnum::GreaterThan => Some(2),
            TokenEnum::LessThan => Some(2),
            TokenEnum::DoubleEquals => Some(2),
            TokenEnum::NotEquals => Some(2),
            TokenEnum::And => Some(1),
            TokenEnum::Or => Some(1),

            TokenEnum::Range(_) => Some(1),
            _ => None
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub(crate) kind: TokenEnum,
    pub span: Span
}

impl GetSpan for Token {
    fn get_span(&self) -> Span {
        self.span.clone()
    }

}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}