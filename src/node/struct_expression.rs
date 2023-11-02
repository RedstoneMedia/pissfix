use crate::{GetSpan, Span};
use crate::node::r#type::TypeExpression;
use crate::token::Token;


#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub field_name: Token,
    pub colon: Token,
    pub field_type: TypeExpression,
}

impl GetSpan for StructField {
    fn get_span(&self) -> Span {
        let start_char = self.field_name.span.start_char;
        let end_char = self.field_type.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructExpression {
    pub keyword: Token,
    pub name: Token,
    pub opening: Token,
    pub fields: Vec<StructField>,
    pub closing: Token,
}

impl GetSpan for StructExpression {
    fn get_span(&self) -> Span {
        let start_char = self.keyword.span.start_char;
        let end_char = self.closing.span.end_char;
        Span {
            start_char,
            end_char,
        }
    }
}