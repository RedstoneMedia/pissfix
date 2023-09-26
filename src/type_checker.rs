use crate::errors::{Error, ErrorKind, ErrorTracker};
use crate::GetSpan;
use crate::node::Node;
use crate::node::prelude::*;
use crate::scope::{AllScopes, Function, Scope, Type};
use crate::token::TokenEnum;

#[derive(Debug, Default)]
pub struct TypeChecker {
    all_scopes: AllScopes
}

impl TypeChecker {

    fn check_function_expression(&mut self, function_expr: &FunctionExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) {
        let TokenEnum::Identifier(function_name) = &function_expr.name.kind else {unreachable!()};
        // If function is already defined in current scope don't re define it.
        if self.all_scopes.get(&current_scope_id).functions.get(function_name).is_some() {
            return;
        }

        // TODO: Create actual error when try_from fails
        let parameters : Vec<_> = function_expr.base.parameters.iter().map(|FunctionParameter { name, parameter_type, .. }| {
            let TokenEnum::Identifier(name) = &name.kind else {unreachable!()};
            let t = Type::try_from(parameter_type).unwrap();
            (name.clone(), t)
        }).collect();

        // TODO: Create actual error when try_from fails
        let return_type = function_expr.base.return_type.as_ref()
            .map(|return_type| Type::try_from(&return_type.return_type).unwrap())
            .unwrap_or(Type::_None);

        self.all_scopes.get_mut(&current_scope_id).functions.insert(function_name.clone(), Function {
            parameters: parameters.clone(),
            returns: return_type.clone(),
            scope_id: Default::default(),
        });
        let mut function_scope = Scope::new(Some(current_scope_id));
        // Add function input arguments to function scope.
        for (param_name, param_type) in parameters {
            function_scope.variables.insert(param_name, param_type);
        }
        let new_scope_id = self.all_scopes.insert(function_scope);

        // Check out function return type
        let mut actual_return_type = self.check_types_recursive(&function_expr.base.body, new_scope_id, error_tracker);
        if !actual_return_type.expect_to_be(&return_type) {
            error_tracker.add_error(Error::from_span(
                function_expr.base.return_type.as_ref()
                         .map(|return_type|return_type.return_type.get_span())
                         .unwrap_or(function_expr.name.get_span()),
                format!("Function returns type: {:?}, but actually has type: {:?}", return_type, actual_return_type),
                ErrorKind::TypeCheckError
            ));
            return;
        }

        let function = self.all_scopes.get_mut(&current_scope_id).functions.get_mut(function_name).unwrap();
        // Set new scope id so the scope can be copied when the function is called
        function.scope_id = new_scope_id;
        function.returns = actual_return_type;
    }

    fn check_assignment_expression(&mut self, assignment_expr: &AssignmentExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) {
        // Check the assignment value
        let expr_type = self.check_types_recursive(&assignment_expr.value, current_scope_id, error_tracker);
        // Completely check the assign to expression (expression before the =)
        match &*assignment_expr.to {
            Node::IdentifierExpression(ident_token) => {
                let TokenEnum::Identifier(identifier_string) = &ident_token.kind else {unreachable!()};
                let insert_scope_variables = &mut self.all_scopes.get_mut(&current_scope_id).variables;
                match insert_scope_variables.insert(identifier_string.clone(), expr_type.clone()) {
                    Some(mut t) => {
                        if !t.expect_to_be(&expr_type) {
                            error_tracker.add_error(Error::from_span(
                                assignment_expr.get_span(),
                                format!("Cannot assign {:?} to variable of type {:?}", expr_type, t),
                                ErrorKind::TypeCheckError
                            ));
                        }
                        *insert_scope_variables.get_mut(identifier_string).unwrap() = t;
                    },
                    _ => {}
                }
            },
            v => eprintln!("Warning: Cannot assign to expression: {:?}", v)
        }
    }

    fn check_call_expression(&mut self, call_expr: &CallExpression, current_scope_id: u64, error_tracker : &mut ErrorTracker) -> Type {
        let TokenEnum::Identifier(function_name) = &call_expr.name.kind else {unreachable!()};

        return match self.all_scopes.find_function_scope_id_in_scope_by_name(current_scope_id, function_name) {
            Some(scope_id) => {
                let function = self.all_scopes
                    .get(&scope_id)
                    .functions.get(function_name)
                    .unwrap();

                let function_arguments_length = function.parameters.len();
                let supplied_arguments_length = call_expr.arguments.len();
                let function_return_type = function.returns.clone();

                if function_arguments_length > supplied_arguments_length {
                    error_tracker.add_error(Error::from_span(
                        call_expr.get_span(),
                        format!("To few arguments, supplied: {}, required: {}", supplied_arguments_length, function_arguments_length),
                        ErrorKind::TypeCheckError
                    ));
                    return function_return_type;
                } else if function_arguments_length < supplied_arguments_length {
                    error_tracker.add_error(Error::from_span(
                        call_expr.get_span(),
                        format!("To many arguments, supplied: {}, required: {}", supplied_arguments_length, function_arguments_length),
                        ErrorKind::TypeCheckError
                    ));
                    return function_return_type;
                }

                let function_parameters = function.parameters.clone();
                for ((_, expected), expr) in function_parameters.iter().zip(call_expr.arguments.iter()) {
                    let mut actual = self.check_types_recursive(expr, current_scope_id, error_tracker);
                    if !actual.expect_to_be(expected) {
                        error_tracker.add_error(Error::from_span(
                            call_expr.get_span(),
                            format!("Function argument type does not match, expected: {:?}, but got: {:?}", expected, actual),
                            ErrorKind::TypeCheckError
                        ));
                        return function_return_type;
                    }
                }
                function_return_type
            },
            None => {
                for expr in &call_expr.arguments {
                    self.check_types_recursive(expr, current_scope_id, error_tracker);
                }

                /*
                TODO: Make standard library functions work
                error_tracker.add_error(Error::from_span(
                    call_expr.name.get_span(),
                    format!("Function is not defined: {}", function_name),
                    ErrorKind::TypeCheckError
                ));*/
                Type::_Unknown
            }
        }
    }

    fn check_identifier_expression(&mut self, identifier_expr: &Token, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Type {
        return match identifier_expr {
            Token { kind: TokenEnum::Identifier(identifier_string), .. } => {
                match self.all_scopes.find_variable_in_scope_by_name(current_scope_id, identifier_string) {
                    Some(t) => {
                        t.clone()
                    },
                    None => {
                        error_tracker.add_error(Error::from_span(
                            identifier_expr.get_span(),
                            format!("Variable: {} does not exits in scope", identifier_string),
                            ErrorKind::TypeCheckError
                        ));
                        Type::_Unknown
                    }
                }
            },
            _ => unreachable!()
        }
    }

    fn check_literal_expression(&mut self, literal_token: &Token) -> Type {
        match literal_token.kind {
            TokenEnum::Number(_) => Type::Integer,
            TokenEnum::FloatLiteral(_) => Type::Float,
            TokenEnum::BooleanLiteral(_) => Type::Boolean,
            TokenEnum::StringLiteral(_) => Type::String,
            _ => unreachable!()
        }
    }

    fn check_binary_expression(&mut self, binary_expr: &BinaryExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Type {
        let mut left_type = self.check_types_recursive(&binary_expr.left, current_scope_id, error_tracker);
        let mut right_type = self.check_types_recursive(&binary_expr.right, current_scope_id, error_tracker);

        match &binary_expr.operation.kind {
            TokenEnum::Range(_) => {
                if !left_type.expect_to_be(&Type::Integer) {
                    error_tracker.add_error(Error::from_span(
                        binary_expr.get_span(),
                        format!("Found non integer types for range: {:?} and {:?}", left_type, right_type),
                        ErrorKind::TypeCheckError
                    ));
                }
                Type::Range
            },
            TokenEnum::And | TokenEnum::Or => match (&left_type, &right_type) {
                (Type::Boolean, Type::Boolean) => Type::Boolean,
                (_, _) => {
                    error_tracker.add_error(Error::from_span(
                        binary_expr.get_span(),
                        format!("Incompatible types for boolean math: {:?} and {:?} ", left_type, right_type),
                        ErrorKind::TypeCheckError
                    ));
                    return Type::union_from(left_type, right_type);
                }
            },
            TokenEnum::DoubleEquals | TokenEnum::NotEquals => {
                if !left_type.expect_to_be(&right_type) {
                    error_tracker.add_error(Error::from_span(
                        binary_expr.get_span(),
                        format!("Comparison types have different types: {:?} and {:?} ", left_type, right_type),
                        ErrorKind::TypeCheckError
                    ));
                }
                Type::Boolean
            }
            TokenEnum::Plus | TokenEnum::Minus | TokenEnum::Multiply | TokenEnum::Divide => {
                // Check if left hand is Float, Integer or String
                if !left_type.expect_to_be(&Type::Union(vec![
                    Type::Float, Type::Integer, Type::String
                ])) {
                    error_tracker.add_error(Error::from_span(
                        binary_expr.left.get_span(),
                        format!("Incompatible left hand type: {:?}", left_type),
                        ErrorKind::TypeCheckError
                    ));
                    return Type::union_from(left_type, right_type);
                }
                // Same but for right hand
                if !right_type.expect_to_be(&Type::Union(vec![
                    Type::Float, Type::Integer, Type::String
                ])) {
                    error_tracker.add_error(Error::from_span(
                        binary_expr.left.get_span(),
                        format!("Incompatible right hand type: {:?}", left_type),
                        ErrorKind::TypeCheckError
                    ));
                    return Type::union_from(left_type, right_type);
                }
                // Check for equality of left and right hand types
                if !left_type.expect_to_be(&right_type) {
                    error_tracker.add_error(Error::from_span(
                        binary_expr.get_span(),
                        format!("Incompatible types: {:?} and {:?} ", left_type, right_type),
                        ErrorKind::TypeCheckError
                    ));
                    return Type::union_from(left_type, right_type);
                }
                left_type
            },
            _ => unreachable!()
        }
    }

    fn check_types_recursive(&mut self, node: &Node, current_scope_id: u64, error_tracker : &mut ErrorTracker) -> Type {
        match node {
            Node::BinaryExpression(binary_expression) => {
                return self.check_binary_expression(binary_expression, current_scope_id, error_tracker);
            },
            Node::UnaryExpression(UnaryExpression { expression, .. }) => {
                return self.check_types_recursive(expression, current_scope_id, error_tracker);
            }
            Node::LiteralExpression(token) => {
                return self.check_literal_expression(token);
            },
            Node::IdentifierExpression(identifier_expression) => {
                return self.check_identifier_expression(identifier_expression, current_scope_id, error_tracker);
            },
            Node::CallExpression(call_expression) => {
                return self.check_call_expression(call_expression, current_scope_id, error_tracker);
            }
            Node::ParenthesizedExpression(inner) => {
                return self.check_types_recursive(inner, current_scope_id, error_tracker)
            }
            Node::AssignmentExpression(assignment_expression) => {
                self.check_assignment_expression(assignment_expression, current_scope_id, error_tracker);
            }
            Node::ExpressionList(ExpressionList { expressions, .. }) => {
                let mut return_types = Vec::with_capacity(expressions.len());
                for expr in expressions {
                    let t = self.check_types_recursive(expr, current_scope_id, error_tracker);
                    if let Type::_None = t {
                        return_types.push(t);
                    }
                }
                return if let Some(t) = return_types.pop() {
                    t
                } else {
                    Type::Tuple(return_types)
                }
            }
            Node::IfExpression(IfExpression { condition, true_branch, false_branch, .. }) => {
                let mut condition_type = self.check_types_recursive(condition, current_scope_id, error_tracker);
                if !condition_type.expect_to_be(&Type::Boolean) {
                    error_tracker.add_error(Error::from_span(
                        condition.get_span(),
                        format!("If condition has to be of type Boolean, but got: {:?}", condition_type),
                        ErrorKind::TypeCheckError
                    ));
                }
                let true_branch_type = self.check_types_recursive(true_branch, current_scope_id, error_tracker);
                if let Some(false_branch) = false_branch {
                    let false_branch_type = self.check_types_recursive(false_branch, current_scope_id, error_tracker);
                    return Type::union_from(true_branch_type, false_branch_type);
                }
                return true_branch_type;
            }
            Node::FunctionExpression(function_expression) => {
                self.check_function_expression(function_expression, current_scope_id, error_tracker);
            },
            Node::AnonymousFunctionExpression(_) => {}
            Node::WhileExpression(WhileExpression { condition, body, .. }) => {
                let mut condition_type = self.check_types_recursive(condition, current_scope_id, error_tracker);
                if !condition_type.expect_to_be(&Type::Boolean) {
                    error_tracker.add_error(Error::from_span(
                        condition.get_span(),
                        format!("While condition has to be of type Boolean, but got: {:?}", condition_type),
                        ErrorKind::TypeCheckError
                    ));
                }
                return self.check_types_recursive(body, current_scope_id, error_tracker)
            }
            Node::ForExpression(ForExpression { iteration_var, iterate_over, body , ..}) => {
                if let TokenEnum::Identifier(iteration_var_name) = &iteration_var.kind {
                    if self.all_scopes.find_variable_scope_id_in_scope_by_name(current_scope_id, iteration_var_name).is_some() {
                        error_tracker.add_error(Error::from_span(
                            iteration_var.get_span(),
                            format!("Reused variable for for loop: {:?}", iteration_var_name),
                            ErrorKind::TypeCheckError
                        ));
                    }
                }
                let mut iterate_over_type = self.check_types_recursive(iterate_over, current_scope_id, error_tracker);
                if !iterate_over_type.expect_to_be(&Type::Union(vec![
                    Type::String,
                    Type::Range,
                    Type::Object,
                    Type::Array(Box::new(Type::_Unknown)),
                ])) {
                    error_tracker.add_error(Error::from_span(
                        iterate_over.get_span(),
                        format!("Can only iterate over String, or Array, but got: {:?}", iterate_over_type),
                        ErrorKind::TypeCheckError
                    ));
                };

                return self.check_types_recursive(body, current_scope_id, error_tracker);
            }
            Node::ReturnExpression(_) => unimplemented!(),
            Node::BreakExpression(_) => {}
            Node::IndexExpression(_) => {
                // TODO: Implement
            }
            Node::CommentExpression(CommentExpression { on , .. }) => {
                if let Some(on) = on {
                    return self.check_types_recursive(on, current_scope_id, error_tracker);
                }
            }
            Node::_Verbatim(_) => unreachable!()
        }
        Type::_None
    }

    pub fn check_types(&mut self, node: &Node, error_tracker : &mut ErrorTracker) {
        let root_scope_id = self.all_scopes.insert(Scope::new(None));
        self.check_types_recursive(node, root_scope_id, error_tracker);
    }

}