use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpression {
    pub keyword: Token,
    pub condition: Box<Node>,
    pub body: Box<Node>
}

impl GetSpan for WhileExpression {
    fn get_span(&self) -> Span {
        let start_char = self.keyword.get_span().start_char;
        let end_char = self.body.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}