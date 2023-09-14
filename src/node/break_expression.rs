use crate::{GetSpan, Span};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct BreakExpression {
    pub keyword: Token,
}

impl GetSpan for BreakExpression {
    fn get_span(&self) -> Span {
        self.keyword.get_span()
    }
}