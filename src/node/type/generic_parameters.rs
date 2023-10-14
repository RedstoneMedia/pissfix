use crate::node::prelude::TypeExpression;
use crate::{GetSpan, Span};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct GenericParameters {
    pub opening: Token,
    pub parameters: Vec<TypeExpression>,
    pub closing: Token
}

impl GetSpan for GenericParameters {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.opening.span.start_char,
            end_char: self.closing.span.end_char,
        }
    }
}