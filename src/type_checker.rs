use std::collections::HashMap;
use rustc_hash::FxHashMap;
use crate::errors::{Error, ErrorKind, ErrorTracker};
use crate::{GetSpan, Span};
use crate::node::Node;
use crate::node::prelude::*;
use crate::r#type::{AllReferences, Generic, TypeRequirement, Type, TypeRequirements, GenericPathSegment};
use crate::scope::{AllScopes, Function, Scope};
use crate::token::TokenEnum;

#[derive(Debug, Clone)]
pub struct Struct {
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub variants: Vec<(String, Option<Type>)>,
}

pub(crate) type DotChainAccessTypes = FxHashMap<usize, Type>;
pub(crate) const ROOT_SCOPE_ID : u64 = 0;

#[derive(Debug)]
pub struct TypeChecker {
    pub(crate) all_scopes: AllScopes,
    pub(crate) all_references: AllReferences,
    pub structs: HashMap<String, Struct>,
    enums: HashMap<String, Enum>,
    function_recursive_calls_check_stack: Vec<(String, Vec<Vec<(Type, Span)>>)>,
    dot_chain_access_stack: Vec<Type>,
    pub dot_chain_access_types: DotChainAccessTypes
}

impl TypeChecker {

    fn unknown(&mut self) -> Type {
        TypeRequirements::new(None, &mut self.all_references)
    }

    fn check_type_expression(&mut self, type_expression: &TypeExpression, generic_types: &Vec<(&str, Type)>, error_tracker: &mut ErrorTracker) -> Type {
        let single_type_expression = match type_expression {
            TypeExpression::SingleTypeExpression(t) => t,
            TypeExpression::LambdaTypeExpression(LambdaTypeExpression { parameters, return_type, .. }) => {
                return Type::Lambda(Box::new(Function {
                    parameters: parameters.types.iter()
                        .enumerate()
                        .map(|(i, t)| (
                            format!("lambda_parameter_{}", i),
                            self.check_type_expression(t, generic_types, error_tracker))
                        )
                        .collect(),
                    generic_parameter_map: Default::default(),
                    returns: return_type
                        .as_ref()
                        .map(|t| self.check_type_expression(&t.return_type, generic_types, error_tracker))
                        .unwrap_or(Type::_None),
                    scope_id: 0,
                }))
            }
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

        if self.structs.get(type_name).is_some() {
            return Type::Struct(type_name.clone());
        }

        if self.enums.get(type_name).is_some() {
            return Type::Enum(type_name.clone());
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
            "Arr" | "Sequence" => {
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
                match type_name.as_ref() {
                    "Arr" => Type::Array(inner_ref_id),
                    "Sequence" => Type::Sequence(inner_ref_id),
                    _ => unreachable!()
                }
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

    pub(crate) fn check_function_head<'a>(&mut self, function_expr: &'a FunctionExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Option<(&'a String, u64)> {
        let TokenEnum::Identifier(function_name) = &function_expr.name.kind else {unreachable!()};
        // If function is already defined in current scope don't re define it.
        if self.all_scopes.get(&current_scope_id).functions.get(function_name).is_some() {
            error_tracker.add_error(Error::from_span(
                function_expr.name.get_span(),
                format!("Functions can only be defined once, but \"{}\" is redefined here", function_name),
                ErrorKind::TypeCheckError
            ));
            return None;
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
        Some((function_name, function_scope_id))
    }

    fn check_function_expression(&mut self, function_expr: &FunctionExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) {
        let Some((function_name, function_scope_id)) = self.check_function_head(function_expr, current_scope_id, error_tracker) else {
            return;
        };

        // Check function body
        self.function_recursive_calls_check_stack.push((function_name.clone(), vec![]));
        let actual_return_type = self.check_types_recursive(&function_expr.base.body, function_scope_id, error_tracker);
        let (_, unchecked_recursive_calls) = self.function_recursive_calls_check_stack.pop().unwrap();
        // Check out function return type
        let function = self.all_scopes.get_mut(&current_scope_id).functions.get_mut(function_name).unwrap();
        if !actual_return_type.expect_to_be(&function.returns, &mut self.all_references, true) {
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
        if !unchecked_recursive_calls.is_empty() {
            let function_clone = function.clone();
            for arguments in unchecked_recursive_calls {
                self.check_call_expression_argument_validity(&function_clone, &arguments, error_tracker);
            }
        }
    }

    fn half_check_base_function_expression(&mut self, base_function_exp: &BaseFunctionExpression, generic_types: &Vec<(&str, Type)>, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Function {
        let parameters : Vec<_> = base_function_exp.parameters.iter().map(|FunctionParameter { name, parameter_type, .. }| {
            let TokenEnum::Identifier(name) = &name.kind else {unreachable!()};
            let param_type = self.check_type_expression(parameter_type, generic_types, error_tracker);
            (name.clone(), param_type)
        }).collect();

        let mut generic_parameter_map = HashMap::with_capacity(generic_types.len());
        for (param_index, (_, param_type)) in parameters.iter().enumerate() {
            let used_generics_paths = param_type.get_used_generics(&self.all_references);
            for mut generic_path in used_generics_paths {
                generic_parameter_map.entry(generic_path.generic_name.clone())
                    .or_insert_with(|| {
                        generic_path.inside(GenericPathSegment::Parameter(param_index));
                        generic_path
                    });
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
        if !actual_return_type.expect_to_be(&function.returns, &mut self.all_references, true) {
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
            },
            Node::DotChainExpression(_) => {
                let to_type = self.check_types_recursive(assignment_expr.to.as_ref(), current_scope_id, error_tracker);
                if !expr_type.expect_to_be(&to_type, &mut self.all_references, false) {
                    error_tracker.add_error(Error::from_span(
                        assignment_expr.value.get_span(),
                        format!(
                            "Cannot assign {} to value of type {}",
                            expr_type.to_string(&self.all_references),
                            to_type.to_string(&self.all_references)
                        ),
                        ErrorKind::TypeCheckError
                    ));
                }
            }
            v => eprintln!("Warning: Cannot assign to expression: {:?}", v)
        }
    }

    fn check_call_expression_argument_validity(
        &mut self,
        function: &Function,
        arguments: &[(Type, Span)],
        error_tracker : &mut ErrorTracker
    ) {
        let real_types : Vec<_> = arguments.iter().map(|(t,_)| t).collect();
        for ((_, expected), (actual, actual_span)) in function.parameters.iter().zip(arguments.iter()) {
            let mut expected_real = expected.clone();
            let replace_result = expected_real.replace_type_generics(&function.generic_parameter_map, &real_types, &mut self.all_references);
            if replace_result.is_err() {
                error_tracker.add_error(Error::from_span(
                    actual_span.clone(),
                    format!("Could not replace generic argument type. Expected: {}, but got: {}", expected_real.to_string(&self.all_references), actual.to_string(&self.all_references)),
                    ErrorKind::TypeCheckError
                ));
                continue;
            }
            if !actual.expect_to_be(&expected_real, &mut self.all_references, true) {
                error_tracker.add_error(Error::from_span(
                    actual_span.clone(),
                    format!("Function argument type does not match, expected: {}, but got: {}", expected_real.to_string(&self.all_references), actual.to_string(&self.all_references)),
                    ErrorKind::TypeCheckError
                ));
            }
        }
    }

    fn check_call_expression(&mut self, call_expr: &CallExpression, current_scope_id: u64, error_tracker : &mut ErrorTracker) -> Type {
        let TokenEnum::Identifier(function_name) = &call_expr.name.kind else {unreachable!()};

        self.all_scopes.find_variable_scope_id_in_scope_by_name(current_scope_id, function_name);


        let function = self.all_scopes.find_function_scope_id_in_scope_by_name(current_scope_id, function_name).
            map(|scope_id| {
                self.all_scopes
                    .get(&scope_id)
                    .functions.get(function_name)
                    .unwrap()
            })
            .or_else(|| {
                let t = self.all_scopes.find_variable_in_scope_by_name(current_scope_id, function_name)?;
                let Type::Lambda(function) = t else {return None};
                Some(function.as_ref())
            }
        );

        return match function {
            Some(function) => {
                let function = function.clone();
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
                let replace_result = function_return_type.replace_type_generics(
                    &function.generic_parameter_map,
                    &arguments.iter()
                        .map(|(t, _)| t)
                        .collect(),
                    &mut self.all_references
                );
                if replace_result.is_err() {
                    error_tracker.add_error(Error::from_span(
                        call_expr.get_span(),
                        format!("Could not replace generic return type."),
                        ErrorKind::TypeCheckError
                    ));
                }
                // Check if function call is done to a function that is currently being checked. Aka a recursive call
                // The argument validly is not describable here, because the called function is not yet fully checked. This is important for generic requirements on the parameters, that might not be known yet.
                if let Some((_, call_infos)) = self.function_recursive_calls_check_stack.iter_mut().find(|(name, _)| function_name == name) {
                    call_infos.push(arguments);
                    return function_return_type;
                }
                // Check if the arguments are valid
                self.check_call_expression_argument_validity(&function, &arguments, error_tracker);
                function_return_type
            },
            None => {
                for expr in &call_expr.arguments {
                    self.check_types_recursive(expr, current_scope_id, error_tracker);
                }
                error_tracker.add_error(Error::from_span(
                    call_expr.name.get_span(),
                    format!("Function is not defined: {}", function_name),
                    ErrorKind::TypeCheckError
                ));
                self.unknown()
            }
        }
    }

    fn check_struct_instantiate_expression(&mut self, struct_instantiate_expression: &StructInstantiateExpression, current_scope_id: u64, error_tracker : &mut ErrorTracker) -> Type {
        let StructInstantiateExpression {name, pairs, ..} = struct_instantiate_expression;
        let TokenEnum::Identifier(struct_name) = name.kind.clone() else {unreachable!()};
        let t_struct = self.structs.get(&struct_name);
        if t_struct.is_none() {
            error_tracker.add_error(Error::from_span(
                name.get_span(),
                format!("Could not find any struct with name: \"{}\"", struct_name),
                ErrorKind::TypeCheckError
            ));
            return Type::_None;
        }
        for pair in pairs {
            let TokenEnum::Identifier(field_name_string) = &pair.field_name.kind else {unreachable!()};
            let actual_initialization_type = self.check_types_recursive(&pair.value, current_scope_id, error_tracker);
            let t_struct = self.structs.get(&struct_name).unwrap();
            let Some((_, expected_type)) = t_struct.fields.iter().find(|(real_field_name, _)| real_field_name == field_name_string) else {
                error_tracker.add_error(Error::from_span(
                    pair.field_name.get_span(),
                    format!("Struct \"{}\" does not have a field with the name: \"{}\"", struct_name, field_name_string),
                    ErrorKind::TypeCheckError
                ));
                continue;
            };
            if !actual_initialization_type.expect_to_be(expected_type, &mut self.all_references, false) {
                error_tracker.add_error(Error::from_span(
                    pair.value.get_span(),
                    format!(
                        "Field \"{}\" on struct \"{}\" has type {}, but got: {}",
                        field_name_string,
                        struct_name,
                        expected_type.to_string(&self.all_references),
                        actual_initialization_type.to_string(&self.all_references)
                    ),
                    ErrorKind::TypeCheckError
                ));
            }
        }
        Type::Struct(struct_name)
    }

    fn check_enum_instantiate_expression(&mut self, enum_instantiate_expression: &EnumInstantiateExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Type {
        let EnumInstantiateExpression { enum_name, variant_name, inner, .. } = enum_instantiate_expression;
        let TokenEnum::Identifier(enum_name_string) = enum_name.kind.clone() else {unreachable!()};
        let actual_inner = inner.as_ref()
            .map(|inner| self.check_types_recursive(inner, current_scope_id, error_tracker));
        let t_enum = self.enums.get(&enum_name_string);
        if t_enum.is_none() {
            error_tracker.add_error(Error::from_span(
                enum_name.get_span(),
                format!("Could not find any enum with name: \"{}\"", enum_name_string),
                ErrorKind::TypeCheckError
            ));
            return Type::_None;
        }
        let t_enum = t_enum.unwrap();
        let TokenEnum::Identifier(variant_name_string) = variant_name.kind.clone() else {unreachable!()};
        let Some((_, expected_inner)) = t_enum.variants.iter()
            .find(|(expected_variant_name, _)| expected_variant_name == &variant_name_string)
            else {
                error_tracker.add_error(Error::from_span(
                    enum_name.get_span(),
                    format!("Enum \"{}\" does not have a variant named: \"{}\"", enum_name_string, variant_name_string),
                    ErrorKind::TypeCheckError
                ));
                return Type::_None;
            };

        if let Some(expected_inner) = expected_inner {
            let Some(actual_inner) = actual_inner else {
                error_tracker.add_error(Error::from_span(
                    variant_name.get_span(),
                    format!("Variant \"{}\" on enum \"{}\" requires inner type, but did not get one", variant_name_string, enum_name_string),
                    ErrorKind::TypeCheckError
                ));
                return Type::Enum(enum_name_string);
            };
            if !actual_inner.expect_to_be(expected_inner, &mut self.all_references, false) {
                error_tracker.add_error(Error::from_span(
                    inner.as_ref().unwrap().get_span(),
                    format!(
                        "Inner value of variant \"{}\" on enum \"{}\" has type {}, but got: {}",
                        variant_name_string,
                        enum_name_string,
                        expected_inner.to_string(&self.all_references),
                        actual_inner.to_string(&self.all_references)
                    ),
                    ErrorKind::TypeCheckError
                ));
            }
        }
        Type::Enum(enum_name_string)
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

        if left.require(left_requirement.clone(), &mut self.all_references, false).is_err() {
            error_tracker.add_error(Error::from_span(
                binary_expr.left.get_span(),
                format!("Type: {} does not support {}", left.to_string(&self.all_references), left_requirement.to_string(&self.all_references)),
                ErrorKind::TypeCheckError
            ));
        };
        if right.require(right_requirement.clone(), &mut self.all_references, false).is_err() {
            error_tracker.add_error(Error::from_span(
                binary_expr.right.get_span(),
                format!("Type: {} does not support {}", right.to_string(&self.all_references), right_requirement.to_string(&self.all_references)),
                ErrorKind::TypeCheckError
            ));
        };
        if !left.expect_to_be(right, &mut self.all_references, false) {
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
        if into_type.require(TypeRequirement::Index(None), &mut self.all_references, false).is_err() {
            error_tracker.add_error(Error::from_span(
                index_expression.index_into.get_span(),
                format!("Type: {} does not support indexing", into_type.to_string(&self.all_references)),
                ErrorKind::TypeCheckError
            ));
            return self.unknown();
        };
        into_type.clone().try_into_iter_inner(&self.all_references).unwrap()
    }

    fn check_inspect_expression(&mut self, inspect_expression: &InspectExpression, current_scope_id: u64, error_tracker: &mut ErrorTracker) -> Type {
        let InspectExpression { on, arms , ..} = inspect_expression;
        let input_type = self.check_types_recursive(on, current_scope_id, error_tracker);
        let mut last_arm_type : Option<Type> = None;
        for arm in arms {
            // Check type selector for validity (And get relevant type information from them)
            let (selected_type, selected_input_type) = match &arm.type_selector {
                InspectTypeSelector::Type(type_expr) => {
                    let t = self.check_type_expression(type_expr, &vec![], error_tracker);
                    (t.clone(), t)
                }
                InspectTypeSelector::EnumVariant { enum_name, variant_name } => {
                    let TokenEnum::Identifier(enum_name_string) = enum_name.kind.clone() else {unreachable!()};
                    let TokenEnum::Identifier(variant_name_string) = &variant_name.kind else {unreachable!()};
                    if let Some(t_enum) = self.enums.get(&enum_name_string) {
                        let found_variant = t_enum.variants.iter().find(|(actual_variant_name, _)|
                            variant_name_string == actual_variant_name
                        );
                        (match found_variant {
                            Some((_, Some(inner_type))) => inner_type.clone(),
                            Some((_, None)) => Type::_None,
                            None => {
                                error_tracker.add_error(Error::from_span(
                                    variant_name.get_span(),
                                    format!("Could not find enum variant {} on enum with name {}", variant_name_string, enum_name_string),
                                    ErrorKind::TypeCheckError
                                ));
                                self.unknown()
                            }
                        }, Type::Enum(enum_name_string))
                    } else {
                        error_tracker.add_error(Error::from_span(
                            enum_name.get_span(),
                            format!("Could not find enum with name {}", enum_name_string),
                            ErrorKind::TypeCheckError
                        ));
                        let t = self.unknown();
                        (t.clone(), t)
                    }
                }
            };
            if !selected_input_type.expect_to_be(&input_type, &mut self.all_references, true) {
                error_tracker.add_error(Error::from_span(
                    arm.type_selector.get_span(),
                    format!(
                        "Impossible to reach type {}, from inspect input type {}",
                        selected_input_type.to_string(&self.all_references),
                        input_type.to_string(&self.all_references)
                    ),
                    ErrorKind::TypeCheckError
                ));
            }
            // Create bind variables and store in new sub scope
            let mut arm_scope_variables : HashMap<String, Type> = HashMap::new();
            if let Some(bind_var_ident) = &arm.bind_var_ident {
                let TokenEnum::Identifier(bind_var_name) = bind_var_ident.kind.clone() else {unreachable!()};
                arm_scope_variables.insert(bind_var_name, selected_type);
            }
            let arm_scope = self.all_scopes.insert(Scope {
                functions: Default::default(),
                variables: arm_scope_variables,
                parent_scope: Some(current_scope_id),
            });
            // Check arm body in sub scope
            let arm_body_type = self.check_types_recursive(&arm.body, arm_scope, error_tracker);
            if let Some(last_arm_type) = last_arm_type {
                if !arm_body_type.expect_to_be(&last_arm_type, &mut self.all_references, false) {
                    error_tracker.add_error(Error::from_span(
                        arm.body.get_span(),
                        format!("Arm return values mismatch: {} and {}", arm_body_type.to_string(&self.all_references), last_arm_type.to_string(&self.all_references)),
                        ErrorKind::TypeCheckError
                    ));
                }
            }
            last_arm_type = Some(arm_body_type);
        }
        last_arm_type.unwrap_or(Type::_None)
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
            Node::StructInstantiateExpression(struct_instantiate_expression) => {
                return self.check_struct_instantiate_expression(struct_instantiate_expression, current_scope_id, error_tracker);
            }
            Node::EnumInstantiateExpression(enum_instantiate_expression) => {
                return self.check_enum_instantiate_expression(enum_instantiate_expression, current_scope_id, error_tracker);
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
                let mut return_type = Type::_None;
                for expr in expressions {
                    let t = self.check_types_recursive(expr, current_scope_id, error_tracker);
                    if let Type::_None = t {} else {
                        if let Type::_None = return_type {} else {
                            error_tracker.add_error(Error::from_span(
                                expr.get_span(),
                                format!("Cannot return multiple values"),
                                ErrorKind::TypeCheckError
                            ));
                        }
                        return_type = t;
                    }
                }
                return return_type;
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
            Node::StructExpression(struct_expression) => {
                let TokenEnum::Identifier(struct_name) = struct_expression.name.kind.clone() else {unreachable!()};
                if self.structs.contains_key(&struct_name) {
                    error_tracker.add_error(Error::from_span(
                        struct_expression.name.get_span(),
                        format!("Struct {} is already defined", struct_name),
                        ErrorKind::TypeCheckError
                    ));
                    return Type::_None;
                }
                let struct_fields : Vec<_> = struct_expression.fields.iter()
                    .map(|field| {
                        let TokenEnum::Identifier(field_name) = field.field_name.kind.clone() else {unreachable!()};
                        let field_type = self.check_type_expression(&field.field_type, &vec![], error_tracker);
                        (field_name, field_type)
                    })
                    .collect();
                self.structs.insert(struct_name, Struct {
                    fields: struct_fields,
                });
            }
            Node::EnumExpression(enum_expression) => {
                let TokenEnum::Identifier(enum_name) = enum_expression.name.kind.clone() else {unreachable!()};
                if self.enums.contains_key(&enum_name) {
                    error_tracker.add_error(Error::from_span(
                        enum_expression.name.get_span(),
                        format!("Enum {} is already defined", enum_name),
                        ErrorKind::TypeCheckError
                    ));
                    return Type::_None;
                }
                let enum_variants: Vec<_> = enum_expression.variants.iter()
                    .map(|variant| {
                        let TokenEnum::Identifier(variant_name) = variant.variant_name.kind.clone() else {unreachable!()};
                        let inner_type = variant.inner.as_ref()
                            .map(|inner| self.check_type_expression(inner, &vec![], error_tracker));
                        (variant_name, inner_type)
                    })
                    .collect();
                self.enums.insert(enum_name, Enum {
                    variants: enum_variants,
                });
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
                let iteration_type = if let Some(t) = iterate_over_type.clone().try_into_iter_inner(&self.all_references) {t} else {
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
            Node::InspectExpression(inspect_expression) => {
                return self.check_inspect_expression(inspect_expression, current_scope_id, error_tracker);
            }
            Node::ReturnExpression(_) => unimplemented!(),
            Node::BreakExpression(_) => {}
            Node::IndexExpression(index_expression) => {
                return self.check_index_expression(index_expression, current_scope_id, error_tracker);
            }
            Node::DotChainExpression(DotChainExpression {expressions}) => {
                let mut expr_iter = expressions.iter();
                let mut last_type = self.check_types_recursive(expr_iter.next().unwrap(), current_scope_id, error_tracker);
                for expr in expr_iter {
                    self.dot_chain_access_stack.push(last_type);
                    last_type = self.check_types_recursive(expr, current_scope_id, error_tracker);
                }
                return last_type;
            }
            Node::DotChainAccess(DotChainAccess {ident, access_id}) => {
                let TokenEnum::Identifier(access_name) = &ident.kind else {unreachable!()};
                let mut access_on = self.dot_chain_access_stack.last().expect("Expected type on access stack!");
                while let Type::Reference(ref_id) = access_on {
                    access_on = self.all_references.get(ref_id);
                }
                return match access_on {
                    Type::Struct(struct_name) => {
                        let t_struct = self.structs.get(struct_name).expect("Expected struct to exist");
                        let Some((_, field_type)) = t_struct.fields.iter()
                            .find(|(field_name, _)| field_name == access_name) else {
                            error_tracker.add_error(Error::from_span(
                                ident.get_span(),
                                format!("Could not find field {} in struct {}", access_name, struct_name),
                                ErrorKind::TypeCheckError
                            ));
                            return self.unknown();
                        };
                        self.dot_chain_access_types.insert(*access_id, access_on.clone()); // Store data for code generator
                        field_type.clone()
                    }
                    _ => {
                        error_tracker.add_error(Error::from_span(
                            ident.get_span(),
                            format!("Can only access on structs, but got: {}", access_on.to_string(&self.all_references)),
                            ErrorKind::TypeCheckError
                        ));
                        self.unknown()
                    }
                }
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

    pub fn new() -> Self {
        let mut new = Self {
            all_scopes: Default::default(),
            all_references: Default::default(),
            structs: Default::default(),
            enums: Default::default(),
            function_recursive_calls_check_stack: vec![],
            dot_chain_access_stack: vec![],
            dot_chain_access_types: Default::default(),
        };
        new.all_scopes.insert(Scope::new(None));
        new
    }

    pub fn check_types(&mut self, node: &Node, error_tracker : &mut ErrorTracker) {
        self.check_types_recursive(node, ROOT_SCOPE_ID, error_tracker);
    }

}