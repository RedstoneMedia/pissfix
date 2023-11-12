use crate::node::prelude::TypeExpression;
use crate::{GetSpan, Span};
use crate::node::base_function_expression::FunctionReturnType;
use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaParameterList {
    pub opening: Token,
    pub types: Vec<TypeExpression>,
    pub closing: Token,
}

impl GetSpan for LambdaParameterList {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.opening.get_span().start_char,
            end_char: self.closing.get_span().start_char,
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct LambdaTypeExpression {
    pub keyword: Token,
    pub opening: Token,
    pub parameters: LambdaParameterList,
    pub return_type: Option<Box<FunctionReturnType>>,
    pub closing: Token,
}

impl GetSpan for LambdaTypeExpression {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.keyword.get_span().start_char,
            end_char: self.closing.get_span().start_char,
        }
    }
}