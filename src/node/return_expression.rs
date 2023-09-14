use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnExpression {
    pub keyword: Token,
    pub expression: Box<Node>
}

impl GetSpan for ReturnExpression {
    fn get_span(&self) -> Span {
        let start_char = self.keyword.get_span().start_char;
        let end_char = self.expression.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}