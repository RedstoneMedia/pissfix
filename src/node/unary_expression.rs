use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operation: Token,
    pub expression: Box<Node>
}

impl GetSpan for UnaryExpression {
    fn get_span(&self) -> Span {
        let start_char = self.operation.get_span().start_char;
        let end_char = self.expression.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}