use bincode::{Decode, Encode};

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

#[derive(Encode, Decode)]
struct StdLibCache {
    std_raw: String,
    root_scope: scope::Scope,
    all_references: r#type::AllReferences
}


const STD_LIB_CACHE_PATH: &str = "std_lib.bin";

pub fn load_std_lib() -> type_checker::TypeChecker {
    let std_raw = include_str!("std_lib/std_headers.piss");
    // Attempt to load from cache
    let cache_path = std::path::Path::new(STD_LIB_CACHE_PATH);
    if cache_path.is_file() {
        let std_lib_bytes = std::fs::read(cache_path)
            .expect("Could not read std_lib cache");
        let (cache, _) : (StdLibCache, _) = bincode::borrow_decode_from_slice(&std_lib_bytes, bincode::config::standard())
            .expect("Could not decode std_lib cache");
        if cache.std_raw == std_raw {
            // Cache is not invalid
            let mut output_type_checker = type_checker::TypeChecker::new();
            let root_scope = output_type_checker.all_scopes.get_mut(&type_checker::ROOT_SCOPE_ID);
            *root_scope = cache.root_scope;
            output_type_checker.all_references = cache.all_references;
            return output_type_checker;
        }
    }
    // Process std_lib from scratch
    let mut error_tracker = errors::ErrorTracker::new();
    let std_headers : String = std_raw.lines()
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
    // Write std_lib to cache
    let cache = StdLibCache {
        std_raw: std_raw.to_string(),
        root_scope: root_scope.clone(),
        all_references: type_checker.all_references.clone(),
    };
    let cache_bytes = bincode::encode_to_vec(cache, bincode::config::standard())
        .expect("Could not encode std_lib cache");
    std::fs::write(cache_path, cache_bytes).expect("Could not write std_lib cache");
    // Construct filled type_checker
    let mut output_type_checker = type_checker::TypeChecker::new();
    let output_root_scope = output_type_checker.all_scopes.get_mut(&type_checker::ROOT_SCOPE_ID);
    std::mem::swap(output_root_scope, root_scope);
    output_type_checker.all_references = type_checker.all_references;
    output_type_checker
}