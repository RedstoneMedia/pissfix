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
mod base_function_expression;
mod r#type;
mod struct_expression;
mod enum_expression;
mod enum_instantiate_expression;
mod struct_instantiate_expression;
mod dot_chain_expression;
mod dot_chain_access;
mod inspect_expression;

pub use crate::{GetSpan, Span};

pub(super) mod prelude {
    pub use crate::node::assignment_expression::AssignmentExpression;
    pub use crate::node::binary_expression::BinaryExpression;
    pub use crate::node::call_expression::CallExpression;
    pub use crate::node::enum_instantiate_expression::EnumInstantiateExpression;
    pub use crate::node::struct_instantiate_expression::{StructInstantiateExpression, StructInitializationPair};
    pub use crate::node::unary_expression::UnaryExpression;
    pub use crate::node::r#type::*;
    pub use crate::node::break_expression::BreakExpression;
    pub use crate::node::expression_list::ExpressionList;
    pub use crate::node::function_expression::{FunctionExpression, FunctionGenericParameters, FunctionGenericParameter};
    pub use crate::node::base_function_expression::{BaseFunctionExpression, FunctionParameter, FunctionReturnType};
    pub use crate::node::dot_chain_expression::{DotChainExpression};
    pub use crate::node::dot_chain_access::{DotChainAccess};
    pub use crate::node::struct_expression::{StructField, StructExpression};
    pub use crate::node::enum_expression::{EnumExpression, EnumVariant};
    pub use crate::node::if_expression::IfExpression;
    pub use crate::node::index_expression::IndexExpression;
    pub use crate::node::return_expression::ReturnExpression;
    pub use crate::node::while_expression::WhileExpression;
    pub use crate::node::for_expression::ForExpression;
    pub use crate::node::inspect_expression::{InspectExpression, InspectTypeSelector, InspectArm};
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
    EnumInstantiateExpression(EnumInstantiateExpression),
    StructInstantiateExpression(StructInstantiateExpression),
    ParenthesizedExpression(Box<Node>),
    AssignmentExpression(AssignmentExpression),
    ExpressionList(ExpressionList),
    IfExpression(IfExpression),
    FunctionExpression(FunctionExpression),
    AnonymousFunctionExpression(BaseFunctionExpression),
    StructExpression(StructExpression),
    EnumExpression(EnumExpression),
    WhileExpression(WhileExpression),
    ForExpression(ForExpression),
    InspectExpression(InspectExpression),
    ReturnExpression(ReturnExpression),
    BreakExpression(BreakExpression),
    IndexExpression(IndexExpression),
    DotChainExpression(DotChainExpression),
    DotChainAccess(DotChainAccess),
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
            Node::StructInstantiateExpression(e) => e.get_span(),
            Node::EnumInstantiateExpression(e) => e.get_span(),
            Node::ParenthesizedExpression(e) => e.get_span(),
            Node::AssignmentExpression(e) => e.get_span(),
            Node::ExpressionList(e) => e.get_span(),
            Node::IfExpression(e) => e.get_span(),
            Node::FunctionExpression(e) => e.get_span(),
            Node::AnonymousFunctionExpression(e) => e.get_span(),
            Node::StructExpression(e) => e.get_span(),
            Node::EnumExpression(e) => e.get_span(),
            Node::WhileExpression(e) => e.get_span(),
            Node::ForExpression(e) => e.get_span(),
            Node::InspectExpression(e) => e.get_span(),
            Node::ReturnExpression(e) => e.get_span(),
            Node::BreakExpression(e) => e.get_span(),
            Node::IndexExpression(e) => e.get_span(),
            Node::DotChainExpression(e) => e.get_span(),
            Node::DotChainAccess(e) => e.get_span(),
            Node::CommentExpression(e) => e.get_span(),
            Node::_Verbatim(_) => panic!("Internal verbatim does not have a associated span"),
        }
    }
}