use crate::{GetSpan, Span};
use crate::node::prelude::BaseFunctionExpression;
use crate::node::type_expression::GenericParameters;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExpression {
    pub keyword: Token,
    pub name: Token,
    pub generic_parameters: Option<GenericParameters>,
    pub base: BaseFunctionExpression,
}

impl GetSpan for FunctionExpression {
    fn get_span(&self) -> Span {
        let start_char = self.keyword.span.start_char;
        let end_char = self.base.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}