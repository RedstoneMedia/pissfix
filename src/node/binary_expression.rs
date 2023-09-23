use std::fmt::{Debug, Formatter};
use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Clone, PartialEq)]
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

impl Debug for BinaryExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:#?},\n{:#?},\n{:#?}", self.left, self.operation, self.right)
        } else {
            write!(f, "{:?} {:?} {:?}", self.left, self.operation, self.right)
        }

    }
}