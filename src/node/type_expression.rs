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


#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpression {
    pub type_name: Token,
    pub generic_parameters: Option<GenericParameters>,
}

impl GetSpan for TypeExpression {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.type_name.span.start_char,
            end_char: self.generic_parameters.as_ref()
                .map(|p| p.get_span().end_char)
                .unwrap_or(self.type_name.span.end_char),
        }
    }
}