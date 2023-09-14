use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
    pub name: Token,
    pub colon: Token,
    pub parameter_type: Token
}

impl GetSpan for FunctionParameter {
    fn get_span(&self) -> Span {
        let start_char = self.name.get_span().start_char;
        let end_char = self.parameter_type.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionReturnType {
    pub arrow: Token,
    pub return_type: Token
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExpression {
    pub keyword: Token,
    pub name: Token,
    pub parameters: Vec<FunctionParameter>,
    pub closing_parenthesis: Token,
    pub return_type: Option<FunctionReturnType>,
    pub body: Box<Node>
}

impl GetSpan for FunctionExpression {
    fn get_span(&self) -> Span {
        let start_char = self.keyword.get_span().start_char;
        let end_char = self.body.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}