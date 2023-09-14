use crate::node::Node;
use crate::{GetSpan, Span};
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub keyword: Token,
    pub condition: Box<Node>,
    pub true_branch: Box<Node>,
    pub false_branch: Option<Box<Node>>
}

impl GetSpan for IfExpression {
    fn get_span(&self) -> Span {
        let start_char = self.keyword.get_span().start_char;
        let end_char = self.false_branch.as_ref()
            .unwrap_or(&self.true_branch)
            .get_span().end_char;
        Span {
            start_char,
            end_char,
        }
    }
}