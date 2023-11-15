use crate::{GetSpan, Span};
use crate::node::Node;
use crate::node::r#type::TypeExpression;
use crate::token::Token;


#[derive(Clone, Debug, PartialEq)]
pub enum InspectTypeSelector {
    Type(TypeExpression),
    EnumVariant {
        enum_name: Token,
        variant_name: Token
    }
}

impl GetSpan for InspectTypeSelector {
    fn get_span(&self) -> Span {
        match self {
            InspectTypeSelector::Type(type_expr) => type_expr.get_span(),
            InspectTypeSelector::EnumVariant { enum_name, variant_name } => {
                Span {
                    start_char: enum_name.span.start_char,
                    end_char: variant_name.span.end_char,
                }
            }
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct InspectArm {
    pub type_selector: InspectTypeSelector,
    pub as_keyword: Option<Token>,
    pub bind_var_ident: Option<Token>,
    pub body: Node,
}

impl GetSpan for InspectArm {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.type_selector.get_span().start_char,
            end_char: self.body.get_span().end_char,
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct InspectExpression {
    pub keyword: Token,
    pub on: Box<Node>,
    pub opening: Token,
    pub arms: Vec<InspectArm>,
    pub closing: Token,
}

impl GetSpan for InspectExpression {
    fn get_span(&self) -> Span {
        Span {
            start_char: self.keyword.span.start_char,
            end_char: self.closing.span.end_char,
        }
    }
}