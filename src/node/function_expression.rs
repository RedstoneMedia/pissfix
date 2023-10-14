use crate::{GetSpan, Span};
use crate::node::prelude::BaseFunctionExpression;
use crate::node::r#type::TypeExpression;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionGenericParameter {
    pub name: Token,
    pub double_point: Option<Token>,
    pub type_restriction: Option<TypeExpression>
}

impl GetSpan for FunctionGenericParameter {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.name.get_span().start_char,
            end_char: self.type_restriction.as_ref()
                .map(|r| r.get_span().end_char)
                .unwrap_or_else(|| self.name.get_span().end_char),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionGenericParameters {
    pub opening: Token,
    pub parameters: Vec<FunctionGenericParameter>,
    pub closing: Token
}

impl GetSpan for FunctionGenericParameters {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.opening.get_span().start_char,
            end_char: self.closing.get_span().end_char,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionExpression {
    pub keyword: Token,
    pub name: Token,
    pub generic_parameters: Option<FunctionGenericParameters>,
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