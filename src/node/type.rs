pub(super) mod union_expression;
pub(super) mod type_expression;
pub(super) mod generic_parameters;

pub use type_expression::SingleTypeExpression;
pub use generic_parameters::GenericParameters;
pub use union_expression::UnionExpression;
use crate::{GetSpan, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpression {
    SingleTypeExpression(SingleTypeExpression),
    UnionTypeExpression(UnionExpression)
}

impl GetSpan for TypeExpression {
    fn get_span(&self) -> Span {
        match self {
            TypeExpression::SingleTypeExpression(e) => e.get_span(),
            TypeExpression::UnionTypeExpression(e) => e.get_span()
        }
    }
}