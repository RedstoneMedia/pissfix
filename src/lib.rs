mod token;
pub mod lexer;
pub mod parser;
pub mod code_generator;
pub mod errors;
mod node;
pub mod type_checker;
mod scope;

#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub struct Span {
    pub start_char : usize,
    pub end_char : usize,
}

pub trait GetSpan {

    fn get_span(&self) -> Span;

}