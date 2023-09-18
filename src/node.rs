mod binary_expression;
mod unary_expression;
mod call_expression;
mod assignment_expression;
mod if_expression;
mod while_expression;
pub mod function_expression;
mod index_expression;
mod expression_list;
mod return_expression;
mod break_expression;
mod comment;
mod for_expression;

pub use crate::{GetSpan, Span};

pub(super) mod prelude {
    pub use crate::node::assignment_expression::AssignmentExpression;
    pub use crate::node::binary_expression::BinaryExpression;
    pub use crate::node::call_expression::CallExpression;
    pub use crate::node::unary_expression::UnaryExpression;
    pub use crate::node::break_expression::BreakExpression;
    pub use crate::node::expression_list::ExpressionList;
    pub use crate::node::function_expression::FunctionExpression;
    pub use crate::node::if_expression::IfExpression;
    pub use crate::node::index_expression::IndexExpression;
    pub use crate::node::return_expression::ReturnExpression;
    pub use crate::node::while_expression::WhileExpression;
    pub use crate::node::for_expression::ForExpression;
    pub use crate::node::comment::CommentExpression;
    pub use crate::token::Token;
}

use prelude::*;


#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    LiteralExpression(Token),
    IdentifierExpression(Token),
    CallExpression(CallExpression),
    ParenthesizedExpression(Box<Node>),
    AssignmentExpression(AssignmentExpression),
    ExpressionList(ExpressionList),
    IfExpression(IfExpression),
    FunctionExpression(FunctionExpression),
    WhileExpression(WhileExpression),
    ForExpression(ForExpression),
    ReturnExpression(ReturnExpression),
    BreakExpression(BreakExpression),
    IndexExpression(IndexExpression),
    CommentExpression(CommentExpression),
    /// Internally used by compiler
    /// Used to directly insert postfix code (which might not be able to be represented by the pissfix ast)
    _Verbatim(String)
}

impl GetSpan for Node {
    fn get_span(&self) -> Span {
        match self {
            Node::BinaryExpression(e) => e.get_span(),
            Node::UnaryExpression(e) => e.get_span(),
            Node::LiteralExpression(e) => e.get_span(),
            Node::IdentifierExpression(e) => e.get_span(),
            Node::CallExpression(e) => e.get_span(),
            Node::ParenthesizedExpression(e) => e.get_span(),
            Node::AssignmentExpression(e) => e.get_span(),
            Node::ExpressionList(e) => e.get_span(),
            Node::IfExpression(e) => e.get_span(),
            Node::FunctionExpression(e) => e.get_span(),
            Node::WhileExpression(e) => e.get_span(),
            Node::ForExpression(e) => e.get_span(),
            Node::ReturnExpression(e) => e.get_span(),
            Node::BreakExpression(e) => e.get_span(),
            Node::IndexExpression(e) => e.get_span(),
            Node::CommentExpression(e) => e.get_span(),
            Node::_Verbatim(_) => panic!("Internal verbatim does not have a associated span"),
        }
    }
}