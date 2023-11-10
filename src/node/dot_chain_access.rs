use crate::{GetSpan, Span};
use crate::token::Token;

/// Basically the same as an Identifier, but with some extra info for the code generator
#[derive(Clone, Debug, PartialEq)]
pub struct DotChainAccess {
    pub ident: Token,
    pub access_id: usize
}

impl GetSpan for DotChainAccess {
    fn get_span(&self) -> Span {
        self.ident.get_span()
    }
}