mod token;
pub mod lexer;
pub mod parser;
pub mod code_generator;
pub mod errors;
mod node;
pub mod type_checker;
mod scope;
mod r#type;

#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub struct Span {
    pub start_char : usize,
    pub end_char : usize,
}

pub trait GetSpan {

    fn get_span(&self) -> Span;

}


pub fn parse_std_lib() -> type_checker::TypeChecker {
    let mut error_tracker = errors::ErrorTracker::new();
    let std_headers : String = include_str!("std_lib/std_headers.piss").lines()
        .map(|line| format!("fun {} {{}}\n", line))
        .collect();
    let mut lexer = lexer::Lexer::new(&std_headers);
    lexer.lex(&mut error_tracker);
    if error_tracker.has_errors() {
        panic!("{}", error_tracker.get_errors_text(&std_headers));
    }
    let mut parser = parser::Parser::new(lexer.tokens);
    let std_root = parser.parse_all(&mut error_tracker);
    if error_tracker.has_errors() {
        panic!("{}", error_tracker.get_errors_text(&std_headers));
    }
    let node::Node::ExpressionList(node::prelude::ExpressionList {expressions, .. }) = std_root else {unreachable!()};
    let mut type_checker = type_checker::TypeChecker::new();
    for expression in expressions {
        let node::Node::FunctionExpression(function_expression) = expression else {unreachable!()};
        type_checker.check_function_head(&function_expression, 0, &mut error_tracker);
    }
    if error_tracker.has_errors() {
        panic!("{}", error_tracker.get_errors_text(&std_headers));
    }
    let root_scope = type_checker.all_scopes.get_mut(&type_checker::ROOT_SCOPE_ID);

    let mut output_type_checker = type_checker::TypeChecker::new();
    let output_all_scopes = output_type_checker.all_scopes.get_mut(&type_checker::ROOT_SCOPE_ID);
    std::mem::swap(output_all_scopes, root_scope);
    output_type_checker.all_references = type_checker.all_references;
    output_type_checker
}