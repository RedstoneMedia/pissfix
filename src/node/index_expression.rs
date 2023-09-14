use crate::node::Node;
use crate::{GetSpan, Span};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpression {
    pub opening_bracket: Token,
    pub index_value: Box<Node>,
    pub index_into: Box<Node>,
    pub closing_bracket: Token,
}

impl GetSpan for IndexExpression {
    fn get_span(&self) -> Span {
        let start_char = self.opening_bracket.get_span().start_char;
        let end_char = self.closing_bracket.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}