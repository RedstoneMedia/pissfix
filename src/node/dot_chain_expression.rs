use crate::{GetSpan, Span};
use super::Node;

#[derive(Clone, PartialEq, Debug)]
pub struct DotChainExpression {
    pub expressions: Vec<Node>
}

impl GetSpan for DotChainExpression {
    fn get_span(&self) -> Span {
        let start_char = self.expressions.first().unwrap().get_span().start_char;
        let end_char = self.expressions.last().unwrap().get_span().end_char;
        Span {
            start_char,
            end_char
        }
    }
}