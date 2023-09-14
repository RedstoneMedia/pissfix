use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Node>,
    pub operation: Token,
    pub right: Box<Node>
}

impl GetSpan for BinaryExpression {
    fn get_span(&self) -> Span {
        let start_char = self.left.get_span().start_char;
        let end_char = self.right.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}