use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionList {
    pub opening: Token,
    pub expressions: Vec<Node>,
    pub closing: Token,
}

impl GetSpan for ExpressionList {
    fn get_span(&self) -> Span {
        let start_char = self.opening.get_span().start_char;
        let end_char = self.closing.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}