use crate::{GetSpan, Span};
use crate::node::Node;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct CommentExpression {
    pub comment: Token,
    pub on: Option<Box<Node>>
}

impl GetSpan for CommentExpression {
    fn get_span(&self) -> Span {
        let start_char = if let Some(on) = self.on.as_ref() {
            on.get_span()
        } else {
            self.comment.get_span()
        }.start_char;
        let end_char = self.comment.get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}