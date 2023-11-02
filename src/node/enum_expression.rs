use crate::{GetSpan, Span};
use crate::node::r#type::TypeExpression;
use crate::token::Token;


#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub variant_name: Token,
    pub inner_paren_opening: Option<Token>,
    pub inner: Option<TypeExpression>,
    pub inner_paren_closing: Option<Token>,
}

impl GetSpan for EnumVariant {
    fn get_span(&self) -> Span {
        let start_char = self.variant_name.span.start_char;
        let end_char = self.inner_paren_closing.as_ref().map(|t| t.get_span().end_char)
            .unwrap_or_else(|| self.variant_name.span.end_char);
        Span {
            start_char,
            end_char,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumExpression {
    pub keyword: Token,
    pub name: Token,
    pub opening: Token,
    pub variants: Vec<EnumVariant>,
    pub closing: Token,
}

impl GetSpan for EnumExpression {
    fn get_span(&self) -> Span {
        let start_char = self.keyword.span.start_char;
        let end_char = self.closing.span.end_char;
        Span {
            start_char,
            end_char,
        }
    }
}