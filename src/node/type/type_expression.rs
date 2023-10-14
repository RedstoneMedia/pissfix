use crate::{GetSpan, Span};
use crate::node::r#type::generic_parameters::GenericParameters;
use crate::token::Token;


#[derive(Debug, Clone, PartialEq)]
pub struct SingleTypeExpression {
    pub type_name: Token,
    pub generic_parameters: Option<GenericParameters>,
}

impl GetSpan for SingleTypeExpression {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.type_name.span.start_char,
            end_char: self.generic_parameters.as_ref()
                .map(|p| p.get_span().end_char)
                .unwrap_or(self.type_name.span.end_char),
        }
    }
}