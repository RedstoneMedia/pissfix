use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub to: Box<Node>,
    pub equals: Token,
    pub value: Box<Node>,
    pub is_operator_equals: bool
}

impl GetSpan for AssignmentExpression {
    fn get_span(&self) -> Span {
        let start_char = self.to.get_span().start_char;
        let end_char = self.value.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}