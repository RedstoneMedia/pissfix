use std::collections::HashMap;
use crate::errors::{Error, ErrorKind, ErrorTracker};
use crate::{GetSpan, Span};
use crate::node::Node;
use crate::node::prelude::*;
use crate::r#type::{AllReferences, Generic, TypeRequirement, Type, TypeRequirements, GenericBase};
use crate::scope::{AllScopes, Function, Scope};
use crate::token::TokenEnum;


#[derive(Debug, Default)]
pub struct TypeChecker {
    all_scopes: AllScopes,
    all_references: AllReferences,
    function_recursive_calls_check_stack: Vec<(String, Vec<Vec<(Type, Span)>>)>
}

impl TypeChecker {

    fn unknown(&mut self) -> Type {
        TypeRequirements::new(None, &mut self.all_references)
    }

    fn check_type_expression(&mut self, type_expression: &TypeExpression, generic_types: &Vec<(&str, Type)>, error_tracker: &mut ErrorTracker) -> Type {
        let single_type_expression = match type_expression {
            TypeExpression::SingleTypeExpression(t) => t,
            TypeExpression::UnionTypeExpression(UnionExpression {types}) => {
                return Type::Union(types.into_iter()
                    .map(|t| self.check_type_expression(t, generic_types, error_tracker))
                    .collect())
            }
        };

        let TokenEnum::Identifier(type_name) = &single_type_expression.type_name.kind else {unreachable!()};
        if let Some(generic_type) = generic_types.iter().find_map(|(generic_name, generic_type)| {
            if generic_name == type_name {
                Some(generic_type)
            } else {
                None
            }
        }) {
            return generic_type.clone();
        }
        match type_name.as_ref() {
            "Int" => Type::Integer,
            "Flt" => Type::Float,
            "Str" => Type::String,
            "Bool" => Type::Boolean,
            "Any" => Type::Any,
            "Lam" => Type::Lambda(Box::new(Function {
                parameters: vec![], // Unknown until generics are properly implemented
                generic_parameter_map: Default::default(),
                returns: self.unknown(), // Unknown until generics are properly implemented
                scope_id: u64::MAX,
            })),
            "Arr" => {
                let inner_type = single_type_expression.generic_parameters
                    .as_ref()
                    .and_then(|p| p.parameters.get(0));
                let Some(inner) = inner_type else {
                    error_tracker.add_error(Error::from_span(
                        single_type_expression.type_name.get_span(),
                        format!("Array needs one generic type argument"),
                        ErrorKind::TypeCheckError
                    ));
                    return self.unknown()
                };
                let inner_type = self.check_type_expression(inner, generic_types, error_tracker);
                let inner_ref_id = self.all_references.insert(inner_type);
                Type::Array(inner_ref_id)
            },
            _ => {
                error_tracker.add_error(Error::from_span(
                    single_type_expression.type_name.get_span(),
                    format!("Invalid type name: {}", type_name),
                    ErrorKind::TypeCheckError
                ));
                self.unknown()
            }
        }
    }

    fn check_function_expression(&mut self, function_expr: &FunctionExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) {
        let TokenEnum::Identifier(function_name) = &function_expr.name.kind else {unreachable!()};
        // If function is already defined in current scope don't re define it.
        if self.all_scopes.get(&current_scope_id).functions.get(function_name).is_some() {
            error_tracker.add_error(Error::from_span(
                function_expr.name.get_span(),
                format!("Functions can only be defined once, but \"{}\" is redefined here", function_name),
                ErrorKind::TypeCheckError
            ));
            return;
        }

        let generic_types = function_expr.generic_parameters.as_ref().map(|g| g.parameters.iter()
            .map(|function_generic_parameter| {
                let TokenEnum::Identifier(generic_name_str) = &function_generic_parameter.name.kind else {unreachable!()};
                if let Some(type_restriction) = &function_generic_parameter.type_restriction {
                    let restricted_type = self.check_type_expression(type_restriction, &vec![], error_tracker);
                    (generic_name_str.as_str(), Generic::new_specific_based(generic_name_str.clone(),  restricted_type))
                } else {
                    (generic_name_str.as_str(), Generic::new_requirement_based(generic_name_str.clone(), None, &mut self.all_references))
                }
            })
            .collect()
        ).unwrap_or(vec![]);
        let function = self.half_check_base_function_expression(&function_expr.base, &generic_types, current_scope_id, error_tracker);
        let function_scope_id = function.scope_id;
        self.all_scopes.get_mut(&current_scope_id)
            .functions
            .insert(function_name.clone(), function);

        // Check function body
        self.function_recursive_calls_check_stack.push((function_name.clone(), vec![]));
        let actual_return_type = self.check_types_recursive(&function_expr.base.body, function_scope_id, error_tracker);
        let (_, unchecked_recursive_calls) = self.function_recursive_calls_check_stack.pop().unwrap();
        // Check out function return type
        let function = self.all_scopes.get_mut(&current_scope_id).functions.get_mut(function_name).unwrap();
        if !actual_return_type.expect_to_be(&function.returns, &mut self.all_references, false) {
            error_tracker.add_error(Error::from_span(
                function_expr.base.return_type.as_ref()
                         .map(|return_type|return_type.return_type.get_span())
                         .unwrap_or(function_expr.name.get_span()),
                format!("Function returns type: {}, but actually has type: {}", function.returns.to_string(&self.all_references), actual_return_type.to_string(&self.all_references)),
                ErrorKind::TypeCheckError
            ));
            return;
        }
        function.returns = actual_return_type;
        // Lastly check recursive calls
        let parameters = function.parameters.clone();
        for arguments in unchecked_recursive_calls {
            self.check_call_expression_argument_validity(&parameters, &arguments, error_tracker);
        }
    }

    fn half_check_base_function_expression(&mut self, base_function_exp: &BaseFunctionExpression, generic_types: &Vec<(&str, Type)>, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Function {
        let parameters : Vec<_> = base_function_exp.parameters.iter().map(|FunctionParameter { name, parameter_type, .. }| {
            let TokenEnum::Identifier(name) = &name.kind else {unreachable!()};
            let param_type = self.check_type_expression(parameter_type, generic_types, error_tracker);
            (name.clone(), param_type)
        }).collect();

        let mut generic_parameter_map = HashMap::with_capacity(generic_types.len());
        for (i, (_, param_type)) in parameters.iter().enumerate() {
            let used_generics = param_type.get_used_generics(&self.all_references);
            for generic_name in used_generics {
                generic_parameter_map.entry(generic_name)
                    .or_insert(i);
            }
        }
        generic_parameter_map.shrink_to_fit();

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
            generic_parameter_map,
            returns: return_type,
            scope_id: new_scope_id,
        }
    }

    fn check_base_function_expression(&mut self, base_function_exp: &BaseFunctionExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Function {
        let mut function = self.half_check_base_function_expression(base_function_exp, &vec![], current_scope_id, error_tracker);
        // Check out function return type
        let actual_return_type = self.check_types_recursive(&base_function_exp.body, function.scope_id, error_tracker);
        // TODO: expect_to_be is not correct here
        if !actual_return_type.expect_to_be(&function.returns, &mut self.all_references, false) {
            error_tracker.add_error(Error::from_span(
                base_function_exp.return_type.as_ref()
                    .map(|return_type|return_type.return_type.get_span())
                    .unwrap_or(Span {
                        start_char: base_function_exp.opening_parenthesis.span.start_char,
                        end_char: base_function_exp.closing_parenthesis.span.end_char
                    }),
                format!("Function returns type: {}, but actually has type: {}", function.returns.to_string(&self.all_references), actual_return_type.to_string(&self.all_references)),
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
                    Some(t) => {
                        if !t.expect_to_be(&expr_type, &mut self.all_references, false) {
                            error_tracker.add_error(Error::from_span(
                                assignment_expr.get_span(),
                                format!("Cannot assign {} to variable of type {}", expr_type.to_string(&self.all_references), t.to_string(&self.all_references)),
                                ErrorKind::TypeCheckError
                            ));
                        }
                        *insert_scope_variables.get_mut(identifier_string).unwrap() = t;
                    },
                    _ => {}
                }
            },
            Node::IndexExpression(index_expression) => {
                let inner_type = self.check_index_expression(index_expression, current_scope_id, error_tracker);
                // Walk down until hitting inevitable identifier expr containing the name of the container variable
                let mut current_expression = &index_expression.index_into;
                while let Node::IndexExpression(IndexExpression { index_into: inner_index_into, ..}) = current_expression.as_ref() {
                    current_expression = inner_index_into;
                }
                let Node::IdentifierExpression(Token {kind: TokenEnum::Identifier(var_name), ..}) = current_expression.as_ref() else {panic!()};
                // Get variable type of found identifier expr
                let insert_scope_variables = &mut self.all_scopes.get_mut(&current_scope_id).variables;
                let Some(container_type) = insert_scope_variables.get_mut(var_name) else {unreachable!("Variable has to exist, because index expression has been checked")};
                // Check inner variable type
                if !inner_type.expect_to_be(&expr_type, &mut self.all_references, false) {
                    error_tracker.add_error(Error::from_span(
                        assignment_expr.value.get_span(),
                        format!(
                                "Cannot assign {} to variable of type {}",
                                expr_type.to_string(&self.all_references),
                                inner_type.to_string(&self.all_references)
                        ),
                        ErrorKind::TypeCheckError
                    ));
                    return;
                }

                if let Type::String = container_type { return; } // String does not have a inner reference type which can be set
                let Type::Reference(inner_id) = inner_type else {unreachable!("Inner array type has to be after Reference")};
                match self.all_references.get(&inner_id) {
                    Type::Generic(_) | Type::Unknown(_) => {}, // Type would already be changed (or not in case of generics)
                    _ => {self.all_references.replace(inner_id, expr_type);}
                }
            }
            v => eprintln!("Warning: Cannot assign to expression: {:?}", v)
        }
    }

    fn check_call_expression_argument_validity(&mut self, parameters: &[(String, Type)], arguments: &[(Type, Span)], error_tracker : &mut ErrorTracker) {
        for ((_, expected), (actual, actual_span)) in parameters.iter().zip(arguments.iter()) {
            if !actual.expect_to_be(expected, &mut self.all_references, true) {
                error_tracker.add_error(Error::from_span(
                    actual_span.clone(),
                    format!("Function argument type does not match, expected: {}, but got: {}", expected.to_string(&self.all_references), actual.to_string(&self.all_references)),
                    ErrorKind::TypeCheckError
                ));
            }
        }
    }

    fn check_call_expression(&mut self, call_expr: &CallExpression, current_scope_id: u64, error_tracker : &mut ErrorTracker) -> Type {
        let TokenEnum::Identifier(function_name) = &call_expr.name.kind else {unreachable!()};

        return match self.all_scopes.find_function_scope_id_in_scope_by_name(current_scope_id, function_name) {
            Some(scope_id) => {
                let function = self.all_scopes
                    .get(&scope_id)
                    .functions.get(function_name)
                    .unwrap()
                    .clone();

                let function_arguments_length = function.parameters.len();
                let supplied_arguments_length = call_expr.arguments.len();
                let mut function_return_type = function.returns.clone();

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
                // Check and get argument types
                let arguments: Vec<_> = call_expr.arguments.iter().map(|expr| {
                    (self.check_types_recursive(expr, current_scope_id, error_tracker), expr.get_span())
                }).collect();
                // Try to replace generic types in return type with known types from the arguments
                function_return_type.replace_type_generics(
                    &function.generic_parameter_map,
                    &arguments.iter()
                        .map(|(t, _)| t)
                        .collect(),
                    &mut self.all_references
                );
                // Check if function call is done to a function that is currently being checked. Aka a recursive call
                // The argument validly is not describable here, because the called function is not yet fully checked. This is important for generic requirements on the parameters, that might not be known yet.
                if let Some((_, call_infos)) = self.function_recursive_calls_check_stack.iter_mut().find(|(name, _)| function_name == name) {
                    call_infos.push(arguments);
                    return function_return_type;
                }
                // Check if the arguments are valid
                self.check_call_expression_argument_validity(&function.parameters, &arguments, error_tracker);
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
                self.unknown()
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
                        self.unknown()
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

    /// Requires that both types are equal and both support the desired operation
    fn binary_require_both(&mut self, left: &Type, right: &Type, binary_expr: &BinaryExpression, error_tracker: &mut ErrorTracker) {
        let (left_requirement, right_requirement) = match binary_expr.operation.kind {
            TokenEnum::DoubleEquals {..} | TokenEnum::NotEquals {..} => (TypeRequirement::Equality(right.clone()), TypeRequirement::Equality(left.clone())),
            TokenEnum::Plus {..} => (TypeRequirement::Addition(right.clone()), TypeRequirement::Addition(left.clone())),
            TokenEnum::Minus {..} => (TypeRequirement::Subtraction(right.clone()), TypeRequirement::Subtraction(left.clone())),
            TokenEnum::Multiply {..} => (TypeRequirement::Multiplication(right.clone()), TypeRequirement::Multiplication(left.clone())),
            TokenEnum::Divide {..} => (TypeRequirement::Division(right.clone()), TypeRequirement::Division(left.clone())),
            TokenEnum::LessThan | TokenEnum::GreaterThan {..} => (TypeRequirement::Ord(right.clone()), TypeRequirement::Ord(left.clone())),
            _ => unreachable!("Not a binary operator token: {:?}", binary_expr.operation.kind)
        };

        if let Err(_) = left.require(left_requirement.clone(), &mut self.all_references, false) {
            error_tracker.add_error(Error::from_span(
                binary_expr.left.get_span(),
                format!("Type: {} does not support {}", left.to_string(&self.all_references), left_requirement.to_string(&self.all_references)),
                ErrorKind::TypeCheckError
            ));
        };
        if let Err(_) = right.require(right_requirement.clone(), &mut self.all_references, false) {
            error_tracker.add_error(Error::from_span(
                binary_expr.right.get_span(),
                format!("Type: {} does not support {}", right.to_string(&self.all_references), right_requirement.to_string(&self.all_references)),
                ErrorKind::TypeCheckError
            ));
        };
        if !left.expect_to_be(&right, &mut self.all_references, false) {
            error_tracker.add_error(Error::from_span(
                binary_expr.get_span(),
                format!("Left and right hand side types are not the same: {} and {}", left.to_string(&self.all_references), right.to_string(&self.all_references)),
                ErrorKind::TypeCheckError
            ));
        }
    }

    fn check_binary_expression(&mut self, binary_expr: &BinaryExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Type {
        let left_type = self.check_types_recursive(&binary_expr.left, current_scope_id, error_tracker);
        let right_type = self.check_types_recursive(&binary_expr.right, current_scope_id, error_tracker);

        match &binary_expr.operation.kind {
            TokenEnum::Range(_) => {
                if !left_type.expect_to_be(&Type::Integer, &mut self.all_references, false) {
                    error_tracker.add_error(Error::from_span(
                        binary_expr.get_span(),
                        format!("Found non integer types for range: {} and {}", left_type.to_string(&self.all_references), right_type.to_string(&self.all_references)),
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
                        format!("Incompatible types for boolean math: {} and {} ", left_type.to_string(&self.all_references), right_type.to_string(&self.all_references)),
                        ErrorKind::TypeCheckError
                    ));
                    Type::Boolean
                }
            },
            TokenEnum::DoubleEquals | TokenEnum::NotEquals | TokenEnum::GreaterThan | TokenEnum::LessThan => {
                self.binary_require_both(&left_type, &right_type, binary_expr, error_tracker);
                Type::Boolean
            }
            TokenEnum::Plus | TokenEnum::Minus | TokenEnum::Multiply | TokenEnum::Divide => {
                self.binary_require_both(&left_type, &right_type, binary_expr, error_tracker);
                left_type
            },
            _ => unreachable!()
        }
    }

    fn check_unary_expression(&mut self, unary_expression: &UnaryExpression,  current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Type {
        let t = self.check_types_recursive(&unary_expression.expression, current_scope_id, error_tracker);
        match unary_expression.operation.kind {
            TokenEnum::Not => {
                if !t.expect_to_be(&Type::Boolean, &mut self.all_references, false) {
                    error_tracker.add_error(Error::from_span(
                        unary_expression.expression.get_span(),
                        format!("Not operation is not supported on type: {}", t.to_string(&self.all_references)),
                        ErrorKind::TypeCheckError
                    ));
                }
               Type::Boolean
            },
            TokenEnum::Minus => {
                if t.require(TypeRequirement::Negation, &mut self.all_references, false).is_err() {
                    error_tracker.add_error(Error::from_span(
                        unary_expression.expression.get_span(),
                        format!("Inversion is not supported on type: {}", t.to_string(&self.all_references)),
                        ErrorKind::TypeCheckError
                    ));
                }
                t
            },
            TokenEnum::Plus => t,
            _ => unreachable!("Invalid unary expression operator")
        }
    }

    fn check_index_expression(&mut self, index_expression: &IndexExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Type {
        let index_type = self.check_types_recursive(&index_expression.index_value, current_scope_id, error_tracker);
        if !index_type.expect_to_be(&Type::Integer, &mut self.all_references, false) {
            error_tracker.add_error(Error::from_span(
                index_expression.index_value.get_span(),
                format!("Indices can only be of type Integer, but got: {}", index_type.to_string(&self.all_references)),
                ErrorKind::TypeCheckError
            ));
        }
        let into_type = self.check_types_recursive(&index_expression.index_into, current_scope_id, error_tracker);
        if let Err(_) = into_type.require(TypeRequirement::Index(None), &mut self.all_references, false) {
            error_tracker.add_error(Error::from_span(
                index_expression.index_into.get_span(),
                format!("Type: {} does not support indexing", into_type.to_string(&self.all_references)),
                ErrorKind::TypeCheckError
            ));
            return self.unknown();
        };
        into_type.clone().try_into_iter_inner(&mut self.all_references).unwrap()
    }

    fn check_types_recursive(&mut self, node: &Node, current_scope_id: u64, error_tracker : &mut ErrorTracker) -> Type {
        match node {
            Node::BinaryExpression(binary_expression) => {
                return self.check_binary_expression(binary_expression, current_scope_id, error_tracker);
            },
            Node::UnaryExpression(unary_expression) => {
                return self.check_unary_expression(unary_expression, current_scope_id, error_tracker);
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
                let mut last_type = self.unknown();
                for expr in expressions {
                    let t = self.check_types_recursive(expr, current_scope_id, error_tracker);
                    if !t.expect_to_be(&last_type, &mut self.all_references, false) {
                        error_tracker.add_error(Error::from_span(
                            expr.get_span(),
                            format!("Expr type does not match array type, got: {}, but should be: {}", t.to_string(&self.all_references), last_type.to_string(&self.all_references)),
                            ErrorKind::TypeCheckError
                        ));
                    }
                    last_type = t;
                }
                let inner_id = if let Type::Reference(ref_id) = last_type {
                    ref_id
                } else {
                    self.all_references.insert(last_type)
                };
                return Type::Array(inner_id);
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
                let condition_type = self.check_types_recursive(condition, current_scope_id, error_tracker);
                if !condition_type.expect_to_be(&Type::Boolean, &mut self.all_references, false) {
                    error_tracker.add_error(Error::from_span(
                        condition.get_span(),
                        format!("If condition has to be of type Boolean, but got: {}", condition_type.to_string(&self.all_references)),
                        ErrorKind::TypeCheckError
                    ));
                }
                let true_branch_type = self.check_types_recursive(true_branch, current_scope_id, error_tracker);
                if let Some(false_branch) = false_branch {
                    let false_branch_type = self.check_types_recursive(false_branch, current_scope_id, error_tracker);
                    if !true_branch_type.expect_to_be(&false_branch_type, &mut self.all_references, false) {
                        error_tracker.add_error(Error::from_span(
                            keyword.get_span(),
                            format!("Branch values mismatch: {} and {}", true_branch_type.to_string(&self.all_references), false_branch_type.to_string(&self.all_references)),
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
                let condition_type = self.check_types_recursive(condition, current_scope_id, error_tracker);
                if !condition_type.expect_to_be(&Type::Boolean, &mut self.all_references, false) {
                    error_tracker.add_error(Error::from_span(
                        condition.get_span(),
                        format!("While condition has to be of type Boolean, but got: {}", condition_type.to_string(&self.all_references)),
                        ErrorKind::TypeCheckError
                    ));
                }
                return self.check_types_recursive(body, current_scope_id, error_tracker)
            }
            Node::ForExpression(ForExpression { iteration_var, iterate_over, body , ..}) => {
                let iterate_over_type = self.check_types_recursive(iterate_over, current_scope_id, error_tracker);
                // Require Iteration
                if iterate_over_type.require(TypeRequirement::Iteration(None), &mut self.all_references, false).is_err() {
                    error_tracker.add_error(Error::from_span(
                        iterate_over.get_span(),
                        format!("Can only iterate over String, or Array, but got: {}", iterate_over_type.to_string(&self.all_references)),
                        ErrorKind::TypeCheckError
                    ));
                }
                let iteration_type = if let Some(t) = iterate_over_type.clone().try_into_iter_inner(&mut self.all_references) {t} else {
                    // Only happens when the require fails
                    self.unknown()
                };
                if let TokenEnum::Identifier(iteration_var_name) = &iteration_var.kind {
                    if self.all_scopes.find_variable_scope_id_in_scope_by_name(current_scope_id, iteration_var_name).is_some() {
                        error_tracker.add_error(Error::from_span(
                            iteration_var.get_span(),
                            format!("Reused variable for for loop: {}", iteration_var_name),
                            ErrorKind::TypeCheckError
                        ));
                    }
                    self.all_scopes.get_mut(&current_scope_id)
                        .variables
                        .insert(iteration_var_name.clone(), iteration_type);
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