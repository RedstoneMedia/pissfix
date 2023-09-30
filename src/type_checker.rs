use crate::errors::{Error, ErrorKind, ErrorTracker};
use crate::{GetSpan, Span};
use crate::node::Node;
use crate::node::prelude::*;
use crate::r#type::Type;
use crate::scope::{AllScopes, Function, Scope};
use crate::token::TokenEnum;

#[derive(Debug, Default)]
pub struct TypeChecker {
    all_scopes: AllScopes
}

impl TypeChecker {

    fn check_type_expression(&mut self, type_expression: &TypeExpression, generic_types: &Vec<TypeExpression>, error_tracker: &mut ErrorTracker) -> Type {
        let TokenEnum::Identifier(type_name) = &type_expression.type_name.kind else {unreachable!()};
        if let Some(_) = generic_types.iter().find(|g| {
            let TokenEnum::Identifier(generic_type_name) = &g.type_name.kind else {unreachable!()};
            generic_type_name == type_name
        }) {
            return Type::Generic(type_name.clone())
        }
        match type_name.as_ref() {
            "Int" => Type::Integer,
            "Flt" => Type::Float,
            "Str" => Type::String,
            "Bool" => Type::Boolean,
            "Obj" => Type::Object,
            "Lam" => Type::Lambda(Box::new(Function {
                parameters: vec![], // Unknown until generics are implemented
                returns: Type::_Unknown, // Unknown until generics are implemented
                scope_id: u64::MAX, // Unknown until generics are implemented
            })),
            "Arr" => {
                let inner_type = type_expression.generic_parameters
                    .as_ref()
                    .map(|p| p.parameters.get(0))
                    .flatten();
                let Some(inner) = inner_type else {
                    error_tracker.add_error(Error::from_span(
                        type_expression.type_name.get_span(),
                        format!("Array needs one generic type argument"),
                        ErrorKind::TypeCheckError
                    ));
                    return Type::_Unknown
                };
                self.check_type_expression(inner, generic_types, error_tracker)
            },
            _ => {
                error_tracker.add_error(Error::from_span(
                    type_expression.type_name.get_span(),
                    format!("Invalid type name: {:?}", type_name),
                    ErrorKind::TypeCheckError
                ));
                Type::_Unknown
            }
        }
    }

    fn check_function_expression(&mut self, function_expr: &FunctionExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) {
        let TokenEnum::Identifier(function_name) = &function_expr.name.kind else {unreachable!()};
        // If function is already defined in current scope don't re define it.
        if self.all_scopes.get(&current_scope_id).functions.get(function_name).is_some() {
            // TODO: Throw error instead of ignoring
            return;
        }

        let empty = Vec::with_capacity(0);
        let generic_types = function_expr.generic_parameters.as_ref().map(|g| &g.parameters).unwrap_or(&empty);
        let function = self.half_check_base_function_expression(&function_expr.base, generic_types, current_scope_id, error_tracker);
        let function_scope_id = function.scope_id;
        self.all_scopes.get_mut(&current_scope_id).functions.insert(function_name.clone(), function);

        // Check out function return type
        let mut actual_return_type = self.check_types_recursive(&function_expr.base.body, function_scope_id, error_tracker);
        let function = self.all_scopes.get_mut(&current_scope_id).functions.get_mut(function_name).unwrap();
        // TODO: expect_to_be is not correct here, as it narrows down the type of unions (Which is not valid here). A fn that specifies -> Int but actually returns Union(Int, String) should not be valid
        if !actual_return_type.expect_to_be(&function.returns) {
            error_tracker.add_error(Error::from_span(
                function_expr.base.return_type.as_ref()
                         .map(|return_type|return_type.return_type.get_span())
                         .unwrap_or(function_expr.name.get_span()),
                format!("Function returns type: {:?}, but actually has type: {:?}", function.returns, actual_return_type),
                ErrorKind::TypeCheckError
            ));
            return;
        }
        function.returns = actual_return_type;
    }

    fn half_check_base_function_expression(&mut self, base_function_exp: &BaseFunctionExpression, generic_types: &Vec<TypeExpression>, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Function {
        let parameters : Vec<_> = base_function_exp.parameters.iter().map(|FunctionParameter { name, parameter_type, .. }| {
            let TokenEnum::Identifier(name) = &name.kind else {unreachable!()};
            let t = self.check_type_expression(parameter_type, generic_types, error_tracker);
            (name.clone(), t)
        }).collect();

        let return_type = base_function_exp.return_type.as_ref()
            .map(|return_type| self.check_type_expression(&return_type.return_type, generic_types, error_tracker))
            .unwrap_or(Type::_None);

        let mut function_scope = Scope::new(Some(current_scope_id));
        // Add function input arguments to function scope.
        for (param_name, param_type) in parameters.iter() {
            function_scope.variables.insert(param_name.clone(), param_type.clone());
        }
        let new_scope_id = self.all_scopes.insert(function_scope);
        Function {
            parameters,
            returns: return_type,
            scope_id: new_scope_id,
        }
    }

    fn check_base_function_expression(&mut self, base_function_exp: &BaseFunctionExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Function {
        let mut function = self.half_check_base_function_expression(base_function_exp, &vec![], current_scope_id, error_tracker);
        // Check out function return type
        let mut actual_return_type = self.check_types_recursive(&base_function_exp.body, function.scope_id, error_tracker);
        // TODO: expect_to_be is not correct here
        if !actual_return_type.expect_to_be(&function.returns) {
            error_tracker.add_error(Error::from_span(
                base_function_exp.return_type.as_ref()
                    .map(|return_type|return_type.return_type.get_span())
                    .unwrap_or(Span {
                        start_char: base_function_exp.opening_parenthesis.span.start_char,
                        end_char: base_function_exp.closing_parenthesis.span.end_char
                    }),
                format!("Function returns type: {:?}, but actually has type: {:?}", function.returns, actual_return_type),
                ErrorKind::TypeCheckError
            ));
        }
        function.returns = actual_return_type;
        function
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
            Node::IndexExpression(index_expression) => {
                self.check_index_expression(index_expression, current_scope_id, error_tracker);
                // Walk down until hitting inevitable identifier expr
                let mut current_expression = &index_expression.index_into;
                let mut depth = 1;
                while let Node::IndexExpression(IndexExpression { index_into: inner_index_into, ..}) = current_expression.as_ref() {
                    current_expression = inner_index_into;
                    depth += 1;
                }
                let Node::IdentifierExpression(Token {kind: TokenEnum::Identifier(identifier_string), ..}) = current_expression.as_ref() else {panic!()};
                // Get variable type of found identifier expr
                let insert_scope_variables = &mut self.all_scopes.get_mut(&current_scope_id).variables;
                let Some(var) = insert_scope_variables.get_mut(identifier_string) else {unreachable!("Variable has to exist, because index expression has been checked")};
                // Get &mut inner type at correct depth
                let mut inner_set_type = var;
                for _ in 0..depth {
                    let Some(t) = inner_set_type.try_into_inner_mut() else {
                        error_tracker.add_error(Error::from_span(
                            assignment_expr.get_span(),
                            format!("Oops"),
                            ErrorKind::TypeCheckError
                        ));
                        return;
                    };
                    inner_set_type = t;
                }
                // Check (and possibly collapse) inner variable type
                if !inner_set_type.expect_to_be(&expr_type) {
                    error_tracker.add_error(Error::from_span(
                        assignment_expr.value.get_span(),
                        format!("Cannot assign {:?} to variable of type {:?}", expr_type, inner_set_type),
                        ErrorKind::TypeCheckError
                    ));
                }
            }
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
                    Type::union_from(left_type, right_type)
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

    fn check_index_expression(&mut self, index_expression: &IndexExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Type {
        let mut index_type = self.check_types_recursive(&index_expression.index_value, current_scope_id, error_tracker);
        if !index_type.expect_to_be(&Type::Integer) {
            error_tracker.add_error(Error::from_span(
                index_expression.index_value.get_span(),
                format!("Indices can only be of type Integer, but got: {:?}", index_type),
                ErrorKind::TypeCheckError
            ));
        }
        let into_type = self.check_types_recursive(&index_expression.index_into, current_scope_id, error_tracker);
        if let Some(inner) = into_type.clone().try_into_iter_inner() {
            inner
        } else {
            error_tracker.add_error(Error::from_span(
                index_expression.index_into.get_span(),
                format!("Can only index into String or Array, but got: {:?}", into_type),
                ErrorKind::TypeCheckError
            ));
            Type::_Unknown
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
            Node::ExpressionList(ExpressionList { opening: Token {kind: TokenEnum::OpeningBracket, ..}, expressions, .. }) => {
                let mut last_type = Type::_Unknown;
                for expr in expressions {
                    let mut t = self.check_types_recursive(expr, current_scope_id, error_tracker);
                    if !t.expect_to_be(&last_type) {
                        error_tracker.add_error(Error::from_span(
                            expr.get_span(),
                            format!("Expr type does not match array type, got: {:?}, but should be: {:?}", t, last_type),
                            ErrorKind::TypeCheckError
                        ));
                    }
                    last_type = t;
                }
                return Type::Array(Box::new(last_type));
            }
            Node::ExpressionList(ExpressionList { expressions, .. }) => {
                let mut return_types = Vec::with_capacity(expressions.len());
                for expr in expressions {
                    let t = self.check_types_recursive(expr, current_scope_id, error_tracker);
                    if let Type::_None = t {} else {
                        return_types.push(t);
                    }
                }
                return if let Some(t) = return_types.pop() {
                    t
                } else {
                    Type::Tuple(return_types)
                }
            }
            Node::IfExpression(IfExpression { keyword, condition, true_branch, false_branch, .. }) => {
                let mut condition_type = self.check_types_recursive(condition, current_scope_id, error_tracker);
                if !condition_type.expect_to_be(&Type::Boolean) {
                    error_tracker.add_error(Error::from_span(
                        condition.get_span(),
                        format!("If condition has to be of type Boolean, but got: {:?}", condition_type),
                        ErrorKind::TypeCheckError
                    ));
                }
                let mut true_branch_type = self.check_types_recursive(true_branch, current_scope_id, error_tracker);
                if let Some(false_branch) = false_branch {
                    let false_branch_type = self.check_types_recursive(false_branch, current_scope_id, error_tracker);
                    if !true_branch_type.expect_to_be(&false_branch_type) {
                        error_tracker.add_error(Error::from_span(
                            keyword.get_span(),
                            format!("Branch values mismatch: {:?} and {:?}", true_branch_type, false_branch_type),
                            ErrorKind::TypeCheckError
                        ));
                    }
                }
                return true_branch_type;
            }
            Node::FunctionExpression(function_expression) => {
                self.check_function_expression(function_expression, current_scope_id, error_tracker);
            },
            Node::AnonymousFunctionExpression(base_function_expression) => {
                return Type::Lambda(Box::new(self.check_base_function_expression(base_function_expression, current_scope_id, error_tracker)))
            }
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
                let iterate_over_type = self.check_types_recursive(iterate_over, current_scope_id, error_tracker);
                let iteration_type = if let Some(it) = iterate_over_type.clone().try_into_iter_inner() {it} else {
                    error_tracker.add_error(Error::from_span(
                        iterate_over.get_span(),
                        format!("Can only iterate over String, or Array, but got: {:?}", iterate_over_type),
                        ErrorKind::TypeCheckError
                    ));
                    Type::_Unknown
                };
                if let TokenEnum::Identifier(iteration_var_name) = &iteration_var.kind {
                    if self.all_scopes.find_variable_scope_id_in_scope_by_name(current_scope_id, iteration_var_name).is_some() {
                        error_tracker.add_error(Error::from_span(
                            iteration_var.get_span(),
                            format!("Reused variable for for loop: {:?}", iteration_var_name),
                            ErrorKind::TypeCheckError
                        ));
                    }
                    self.all_scopes.get_mut(&current_scope_id).variables.insert(iteration_var_name.clone(), iteration_type);
                }
                return self.check_types_recursive(body, current_scope_id, error_tracker);
            }
            Node::ReturnExpression(_) => unimplemented!(),
            Node::BreakExpression(_) => {}
            Node::IndexExpression(index_expression) => {
                return self.check_index_expression(index_expression, current_scope_id, error_tracker);
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