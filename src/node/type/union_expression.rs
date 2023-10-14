use crate::node::prelude::TypeExpression;
use crate::{GetSpan, Span};

#[derive(Debug, Clone, PartialEq)]
pub struct UnionExpression {
    pub types: Vec<TypeExpression>,
}

impl GetSpan for UnionExpression {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.types.first().unwrap().get_span().start_char,
            end_char: self.types.last().unwrap().get_span().start_char,
        }
    }
}