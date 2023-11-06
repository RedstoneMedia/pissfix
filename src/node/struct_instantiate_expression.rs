use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;


#[derive(Debug, Clone, PartialEq)]
pub struct StructInitializationPair {
    pub field_name: Token,
    pub colon: Token,
    pub value: Node,
}

impl GetSpan for StructInitializationPair {
    fn get_span(&self) -> Span {
        let start_char = self.field_name.span.start_char;
        let end_char = self.value.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInstantiateExpression {
    pub name: Token,
    pub opening: Token,
    pub pairs: Vec<StructInitializationPair>,
    pub closing: Token,
}

impl GetSpan for StructInstantiateExpression {
    fn get_span(&self) -> Span {
        let start_char = self.name.span.start_char;
        let end_char = self.closing.span.end_char;
        Span {
            start_char,
            end_char,
        }
    }
}