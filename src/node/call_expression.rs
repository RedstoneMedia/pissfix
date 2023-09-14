use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub name: Token,
    pub closing_parenthesis: Token,
    pub arguments: Vec<Node>,
}

impl GetSpan for CallExpression {
    fn get_span(&self) -> Span {
        let start_char = self.name.get_span().start_char;
        let end_char = self.closing_parenthesis.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}