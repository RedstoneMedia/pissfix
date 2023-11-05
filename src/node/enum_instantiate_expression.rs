use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct EnumInstantiateExpression {
    pub enum_name: Token,
    pub colon: Token,
    pub variant_name: Token,
    pub inner_opening: Option<Token>,
    pub inner: Option<Box<Node>>,
    pub inner_closing: Option<Token>,
}

impl GetSpan for EnumInstantiateExpression {
    fn get_span(&self) -> Span {
        let start_char = self.enum_name.span.start_char;
        let end_char = self.inner_closing.as_ref()
            .map(|t| t.get_span().end_char)
            .unwrap_or_else(|| self.variant_name.get_span().end_char);
        Span {
            start_char,
            end_char,
        }
    }
}