use std::collections::HashMap;
use crate::node::{Node, prelude::*};
use crate::r#type::Type;
use crate::token::TokenEnum;
use crate::type_checker::{DotChainAccessTypes, Struct};

const INDENT: &str = "    ";
const MAX_LINE_LENGTH: usize = 60;

static FUNCTION_REPLACEMENTS: once_cell::sync::Lazy<HashMap<&'static str, &'static str>> = once_cell::sync::Lazy::new(|| {
    include_str!("std_lib/replaced_names.txt").lines()
        .map(|line| {
            let (original, replaced) = line.split_once(' ').unwrap();
            (replaced, original)
        })
        .collect()
});

#[derive(Default)]
pub struct CodeGenerator {
    pub code: String,
    dot_chain_access_types: DotChainAccessTypes,
    structs: HashMap<String, Struct>
}

impl CodeGenerator {
    fn add_with_indent(&mut self, mut add: &str, mut indent_level: usize) {
        if add.is_empty() {return;}
        if !self.code.ends_with('\n') {
            indent_level = 0;
        } else if !self.code.ends_with(' ') {
            add = add.trim_start();
        }
        let indent_level = (indent_level as isize - 1).max(0) as usize;
        self.code += &(INDENT.repeat(indent_level) + add);
        // Add auto line break after hitting MAX_LINE_LENGTH
        let current_line_length = self.code.chars()
            .rev()
            .take_while(|c| c != &'\n')
            .count();
        if current_line_length > MAX_LINE_LENGTH && !add.ends_with('\n') {
            self.code += "\n";
        }
    }

    fn get_type_expression_code<'a>(type_expression: &'a TypeExpression, generic_types: &[&str]) -> &'a str {
        match type_expression {
            TypeExpression::SingleTypeExpression(SingleTypeExpression { type_name: type_name_token, .. }) => {
                let TokenEnum::Identifier(type_name) = &type_name_token.kind else {unreachable!()};
                if generic_types.contains(&type_name.as_str()) || type_name == "Sequence" {"Obj"} else {type_name}
            },
            TypeExpression::LambdaTypeExpression(_) => "Lam",
            TypeExpression::UnionTypeExpression(_) => "Obj"
        }
    }

    fn fill_index_assignment_value_path<'a>(index_into: &'a Node, value_path_expressions: &mut Vec<&'a Box<Node>>) -> (&'a Node, Option<Node>, &'static str) {
        // This is required as a normally generating the code for a index expression would modify a copy of the gotten value, which is unwanted and might be invalid
        let mut current_expression = index_into;
        while let Node::IndexExpression(IndexExpression { index_value: inner_index_value, index_into: inner_index_into, ..}) = current_expression {
            value_path_expressions.push(inner_index_value);
            current_expression = inner_index_into;
        }
        // Use set with simple index or path-set with array of indices for nested index expressions
        // We cannot use path-set for everything, as it only works on arrays while set also works on strings
        let (index_value_expr, function_name) = if value_path_expressions.len() == 1 {
            (None, "set")
        } else {
            let path_array_expression_list = Node::ExpressionList(ExpressionList {
                opening: Token { kind: TokenEnum::OpeningBracket, span: Default::default() },
                expressions: value_path_expressions
                    .into_iter()
                    .rev() // Important as the AST starts from the outer most indexing expression, which is the last one
                    .map(|n| n.as_ref().clone())
                    .collect(),
                closing: Token { kind: TokenEnum::ClosingBracket, span: Default::default() },
            });
            (Some(path_array_expression_list), "path-set")
        };
        (current_expression, index_value_expr, function_name)
    }

    fn get_dot_chain_access_base_name(&self, expr: &Node) -> String {
        let Node::DotChainAccess(DotChainAccess { ident, access_id }) = expr else {unreachable!()};
        let TokenEnum::Identifier(property_name) = &ident.kind else {unreachable!()};
        let Type::Struct(struct_name) = self.dot_chain_access_types.get(access_id).unwrap() else {unimplemented!("DotChainAccess is only implemented for structs")};
        format!("{}-{}", struct_name.to_lowercase(), property_name)
    }

    fn generate_base_function_code(&mut self, anonymous_function_expression: &BaseFunctionExpression, generic_parameters: &Option<FunctionGenericParameters>, indent_level: usize) {
        let BaseFunctionExpression {parameters, return_type, body, ..} = anonymous_function_expression;
        self.add_with_indent("(", indent_level);
        let generic_types = if let Some(generic_parameters) = generic_parameters {
            generic_parameters.parameters.iter().map(|generic_parm| {
                let TokenEnum::Identifier(generic_type_name) = &generic_parm.name.kind else {unreachable!()};
                generic_type_name.as_str()
            }).collect()
        } else {vec![]};
        for parameter in parameters {
            let TokenEnum::Identifier(parameter_name) = &parameter.name.kind else {unreachable!()};
            let parameter_type = CodeGenerator::get_type_expression_code(&parameter.parameter_type, &generic_types);
            self.add_with_indent(&format!("{} :{}, ", parameter_name, parameter_type), indent_level);
        }
        self.code.pop(); // Removes unnecessary trailing ' '
        self.code.pop(); // Removes unnecessary trailing ','
        if let Some(return_type) = return_type {
            let return_type = CodeGenerator::get_type_expression_code(&return_type.return_type, &generic_types);
            self.add_with_indent(&format!(" -> :{}", return_type), indent_level);
        };
        self.add_with_indent(") ", indent_level);
        self.generate_code(body, indent_level);
    }

    fn generate_assignment_code(&mut self, assignment_expression: &AssignmentExpression, indent_level: usize) {
        let AssignmentExpression {to, value, ..} = assignment_expression;
        match &**to {
            Node::IndexExpression(IndexExpression {index_value, index_into, ..}) => {
                // This is required as a normally generating the code for a index expression would modify a copy of the gotten value, which is unwanted and might be invalid
                let mut value_path_expressions = vec![index_value];
                let (most_inner_expression, path_index_value, function_name) = Self::fill_index_assignment_value_path(index_into, &mut value_path_expressions);
                if let Node::IdentifierExpression(_) = most_inner_expression {} else { panic!("First index into always has to be a identifier expression") }
                self.generate_code(most_inner_expression, indent_level);
                self.add_with_indent(" ", indent_level);
                // Generate code for index value
                if let Some(path_index_value) = path_index_value {
                    self.generate_code(&path_index_value, indent_level);
                } else {
                    self.generate_code(index_value, indent_level);
                }
                // Add assignment value code
                self.add_with_indent(" ", indent_level);
                self.generate_code(value, indent_level);
                // Assign with either "set" or "path-set"
                self.add_with_indent(&format!(" {} ", function_name), indent_level);
                self.generate_code(most_inner_expression, indent_level);
            }
            Node::DotChainExpression(DotChainExpression {expressions}) => {
                let mut expressions_iter = expressions.iter();
                let Node::IdentifierExpression(Token {kind: TokenEnum::Identifier(assign_to_var), ..}) = expressions_iter.next().unwrap() else {unreachable!()};
                let mut temp_assignment_var = assign_to_var.clone();
                let mut set_functions = vec![];
                // Generate code to go down the dot chain pushing the Left assign values to the stack
                for (i, expr) in expressions_iter.enumerate() {
                    // Add the last value to get from to the stack for the set to work
                    self.add_with_indent(&temp_assignment_var, indent_level);
                    self.add_with_indent("\n", indent_level);
                    // Handel indexing in dot chain assignment
                    if let Node::IndexExpression(_) = expr {
                        let mut value_path_expressions = vec![];
                        let (most_inner_expr, index_value, index_set_function) = Self::fill_index_assignment_value_path(expr, &mut value_path_expressions);
                        // Generate the code to put the array of the current expression in the dot chain on the stack
                        // The most inner expr in the index chain is always the left most expression.
                        // Since we are in a dot chain this is the property that contains the array we are currently indexing on.
                        self.add_with_indent(&temp_assignment_var, indent_level);
                        self.add_with_indent(" ", indent_level);
                        self.generate_code(most_inner_expr, indent_level);
                        self.add_with_indent("\n", indent_level);
                        // Generate code to push index value for index set function
                        if let Some(index_value) = index_value {
                            self.generate_code(&index_value, indent_level);
                        } else {
                            self.generate_code(value_path_expressions.first().unwrap(), indent_level);
                        }
                        self.add_with_indent(" ", indent_level);
                        // Add set_functions for outer and inner array set
                        set_functions.push(self.get_dot_chain_access_base_name(most_inner_expr) + "-set");
                        set_functions.push(index_set_function.to_string());
                    } else {
                        set_functions.push(self.get_dot_chain_access_base_name(expr) + "-set");
                    }
                    if i == expressions.len() - 2 { break; } // Don't bother generating code for the next expression in the dot chain, if at the end of it
                    // Generate the code to access the inner value from the current expression, with the last value
                    self.add_with_indent(&temp_assignment_var, indent_level);
                    self.add_with_indent(" ", indent_level);
                    self.generate_code(expr, indent_level);
                    // Save inner value to var
                    let new_temp_assignment_var = format!("temp_assign_{}", i);
                    self.add_with_indent(&format!(" {}!\n", new_temp_assignment_var), indent_level);
                    temp_assignment_var = new_temp_assignment_var;
                }
                // Generate code to push assignment value to stack
                self.generate_code(value, indent_level);
                self.add_with_indent(" ", indent_level);
                // Generate code to go up the dot chain setting the values from the bottom up
                for set_function in set_functions.iter().rev() {
                    self.add_with_indent(set_function, indent_level);
                    self.add_with_indent("\n", indent_level);
                }
                self.add_with_indent(assign_to_var, indent_level);
            },
            _ => {
                self.generate_code(value, indent_level);
                self.add_with_indent(" ", indent_level);
                self.generate_code(to, indent_level);
            }
        }
        self.add_with_indent("!", indent_level);
    }

    fn generate_type_check_code(&mut self, type_expression: &TypeExpression, indent_level: usize) {
        match type_expression {
            TypeExpression::SingleTypeExpression(SingleTypeExpression { type_name, generic_parameters }) => {
                let TokenEnum::Identifier(type_name_string) = &type_name.kind else {unreachable!()};
                match type_name_string.as_str() {
                    "Str" => self.add_with_indent("str?", indent_level),
                    "Int" => self.add_with_indent("int?", indent_level),
                    "Flt" => self.add_with_indent("flt?", indent_level),
                    "Bool" => self.add_with_indent("bool?", indent_level),
                    "Arr" | "Sequence" => {
                        self.add_with_indent("dup ", indent_level);
                        match type_name_string.as_str() {
                            "Arr" => self.add_with_indent("arr?", indent_level),
                            "Sequence" => self.add_with_indent("dup arr? swap str? or", indent_level),
                            _ => unreachable!(),
                        }
                        // Check if it is a array/sequence
                        self.add_with_indent(" {\n", indent_level+1);
                        let Some(inner_type) = generic_parameters
                            .as_ref()
                            .and_then(|p| p.parameters.get(0)) else {unreachable!()};
                        // If it is one check if every item in the array/sequence has the inner type
                        self.add_with_indent("true swap {\n", indent_level+1);
                        self.generate_type_check_code(inner_type, indent_level+2);
                        self.add_with_indent("and\n", indent_level+2);
                        self.add_with_indent("} for\n", indent_level+1);
                        // If it is not a array/sequence, then we don't have the correct type
                        self.add_with_indent("} {pop false} if", indent_level);
                    },
                    "Any" => self.add_with_indent("pop true", indent_level),
                    _ => unreachable!("Unknown type {}", type_name_string)
                }
            },
            TypeExpression::LambdaTypeExpression(_) => unimplemented!("PostFix has no way to check if a value is a lambda function"),
            TypeExpression::UnionTypeExpression(UnionExpression { types }) => {
                self.add_with_indent("tmp! ", indent_level);
                // Push (if every type is fulfilled) to the stack
                for t in types {
                    self.add_with_indent("tmp ", indent_level);
                    self.generate_type_check_code(t, indent_level);
                }
                // Return true if any of the pushed values are true
                for _ in 0..(types.len() - 1) {
                    self.add_with_indent("or ", indent_level);
                }
                self.code.pop(); // Remove trailing " "
            }
        }
        self.add_with_indent(" ", indent_level);
    }

    fn generate_inspect_code(&mut self, inspect_expression: &InspectExpression, indent_level: usize) {
        let InspectExpression {on, arms , ..} = inspect_expression;
        self.generate_code(on, indent_level);
        self.add_with_indent(" inspect_on!\n", indent_level);
        self.add_with_indent("[\n", indent_level);
        for arm in arms {
            // Add code to check if type matches type selector
            self.add_with_indent("{", indent_level+1);
            self.add_with_indent("inspect_on ", indent_level+1);
            match &arm.type_selector {
                InspectTypeSelector::Type(type_expression) => {
                    self.generate_type_check_code(type_expression, indent_level + 1);
                    self.code.pop(); // Remove trailing space
                }
                InspectTypeSelector::EnumVariant { variant_name: variant_name_ident, ..} => {
                    let TokenEnum::Identifier(variant_name) = &variant_name_ident.kind else {unreachable!()};
                    self.add_with_indent(&format!("{}_?", variant_name.to_lowercase()), indent_level+1);
                }
            }
            self.add_with_indent("} ", indent_level+1);

            // Add arm body (with or without binding variable)
            if let Some(bind_var_ident) = &arm.bind_var_ident {
                let TokenEnum::Identifier(bind_var_string) = &bind_var_ident.kind else {unreachable!()};
                // If we have a enum variant selector get the inner variant value
                let inner_getter = if let InspectTypeSelector::EnumVariant {variant_name: variant_name_ident, ..} = &arm.type_selector {
                    let TokenEnum::Identifier(variant_name) = &variant_name_ident.kind else {unreachable!()};
                    format!("inspect_on {}_-inner", variant_name.to_lowercase())
                } else {
                    "inspect_on".to_string()
                };
                let bind_inner_verbatim = Node::_Verbatim(format!("{} {}! ", inner_getter, bind_var_string));
                let expressions = if let Node::ExpressionList(ExpressionList {opening: Token {kind: TokenEnum::OpeningBrace, ..}, expressions, ..}, ) = &arm.body {
                    let mut modified_expressions = Vec::with_capacity(expressions.len() + 1);
                    modified_expressions.push(bind_inner_verbatim);
                    modified_expressions.extend_from_slice(expressions);
                    modified_expressions
                } else {
                    vec![bind_inner_verbatim, arm.body.clone()]
                };
                let arm_body = Node::ExpressionList(ExpressionList {
                    opening: Token { kind: TokenEnum::OpeningBrace, span: Default::default() },
                    expressions,
                    closing: Token { kind: TokenEnum::ClosingBrace, span: Default::default() },
                });
                self.generate_code(&arm_body, indent_level+1);
            } else {
                self.generate_code(&arm.body, indent_level+1);
            }
            self.add_with_indent("\n", indent_level);
        }
        self.add_with_indent("] cond\n", indent_level);
    }

    pub fn generate_code(&mut self, node: &Node, indent_level: usize) {
        match node {
            Node::BinaryExpression(BinaryExpression {left, right, operation}) => {
                self.generate_code(left, indent_level);
                self.add_with_indent(" ", indent_level);
                self.generate_code(right, indent_level);

                let operation = match operation.kind {
                    TokenEnum::Plus => "+",
                    TokenEnum::Minus => "-",
                    TokenEnum::Multiply => "*",
                    TokenEnum::Divide => "/",
                    TokenEnum::GreaterThan => ">",
                    TokenEnum::LessThan => "<",
                    TokenEnum::And => "and",
                    TokenEnum::Or => "or",
                    TokenEnum::DoubleEquals => "=",
                    TokenEnum::NotEquals => "!=",
                    TokenEnum::Range(inclusive) => if inclusive {
                        "1 +"
                    } else {
                        return;
                    }
                    _ => unreachable!()
                };
                let operation = format!(" {}\n", operation);
                self.add_with_indent(&operation, indent_level);
            }
            Node::UnaryExpression(UnaryExpression { operation, expression }) => {
                self.generate_code(expression, indent_level);
                let operation = match operation.kind {
                    TokenEnum::Plus => "",
                    TokenEnum::Minus => "-1 *",
                    TokenEnum::Not => "not",
                    _ => unreachable!("")
                };
                let operation = String::from(' ') + operation;
                self.add_with_indent(&operation, indent_level);
            }
            Node::LiteralExpression(token) => {
                let literal = match &token.kind {
                    TokenEnum::Number(number) => format!("{}", number),
                    TokenEnum::FloatLiteral(float) => {
                        let mut float_string = format!("{}", float);
                        if !float_string.contains('.') {
                            float_string += ".0"
                        }
                        float_string
                    },
                    TokenEnum::BooleanLiteral(boolean) => format!("{}", boolean),
                    TokenEnum::StringLiteral(string) => format!("\"{}\"", string.replace('"', "\\\"")),
                    _ => unreachable!()
                };
                self.add_with_indent(&literal, indent_level);
            }
            Node::IdentifierExpression(token) => {
                let TokenEnum::Identifier(ident) = &token.kind else {unreachable!()};
                self.add_with_indent(ident, indent_level);
            }
            dot_chain_expr@Node::DotChainAccess(_) => {
                let access_function_name = self.get_dot_chain_access_base_name(dot_chain_expr);
                self.add_with_indent(&access_function_name, indent_level);
            }
            Node::CallExpression(CallExpression { name, arguments, .. }) => {
                for argument in arguments {
                    self.generate_code(argument, indent_level);
                    self.add_with_indent(" ", indent_level);
                }
                let TokenEnum::Identifier(ident) = &name.kind else {unreachable!()};
                self.add_with_indent(&replace_function_ident(ident), indent_level);
            }
            Node::StructInstantiateExpression(StructInstantiateExpression { name, pairs, .. }) => {
                let TokenEnum::Identifier(name_string) = &name.kind else {unreachable!()};
                let t_struct = self.structs.get(name_string).unwrap();
                // The order in the instantiation might not be the same as in the definition of the struct
                let mut indexed_pairs : Vec<_> = pairs.iter().map(|pair| {
                    let TokenEnum::Identifier(instantiation_field_name) = &pair.field_name.kind else {unreachable!()};
                    let (field_index, _) = t_struct.fields.iter()
                        .enumerate()
                        .find(|(_, (field_name, _))| field_name == instantiation_field_name)
                        .unwrap();
                    (field_index, pair)
                }).collect();
                indexed_pairs.sort_by_key(|(i, _)| *i);
                // Add pairs in correct order
                for (_, pair) in indexed_pairs {
                    self.generate_code(&pair.value, indent_level);
                    self.add_with_indent(" ", indent_level);
                }
                self.add_with_indent(&name_string.to_lowercase(), indent_level);
            }
            Node::EnumInstantiateExpression(EnumInstantiateExpression {variant_name, inner, .. }) => {
                let TokenEnum::Identifier(mut variant_name) = variant_name.kind.clone() else {unreachable!()};
                if let Some(inner) = inner {
                    self.generate_code(inner, indent_level);
                    self.add_with_indent(" ", indent_level);
                    variant_name += "_";
                }
                self.add_with_indent(&variant_name.to_lowercase(), indent_level);
            }
            Node::ParenthesizedExpression(node) => self.generate_code(node, indent_level),
            Node::AssignmentExpression(assignment_expression) => {
                self.generate_assignment_code(assignment_expression, indent_level);
            },
            Node::ExpressionList(ExpressionList {expressions, opening, ..}) => {
                let (start, end) = match opening.kind {
                    TokenEnum::OpeningBrace => ("{\n", "}"),
                    TokenEnum::OpeningBracket => ("[\n", "]"),
                    TokenEnum::OpeningParentheses => ("(\n", ")"),
                    TokenEnum::NoToken => ("", ""),
                    _ => unreachable!()
                };

                self.add_with_indent(start, indent_level);
                for expression in expressions {
                    self.generate_code(expression, indent_level + 1);
                    self.add_with_indent("\n", indent_level);
                }
                self.add_with_indent(end, indent_level);
            }
            Node::IfExpression(IfExpression {condition, true_branch, false_branch, .. }) => {
                self.generate_code(condition, indent_level);
                self.add_with_indent(" ", indent_level);
                self.generate_code(true_branch, indent_level);
                if let Some(false_branch) = false_branch {
                    self.generate_code(false_branch, indent_level);
                };
                self.add_with_indent(" if", indent_level);
            }
            Node::InspectExpression(inspect_expression) => {
                self.generate_inspect_code(inspect_expression ,indent_level);
            }
            Node::FunctionExpression(FunctionExpression { name, base, generic_parameters, ..}) => {
                let TokenEnum::Identifier(name_ident) = &name.kind else {unreachable!()};
                let name_ident = replace_function_ident(name_ident);
                self.add_with_indent(&format!(":{}", name_ident), indent_level);
                self.generate_base_function_code(base, generic_parameters, indent_level);
                self.add_with_indent(" fun\n\n", indent_level);
            }
            Node::AnonymousFunctionExpression(base) => {
                self.generate_base_function_code(base, &None, indent_level);
                self.add_with_indent(" lam\n", indent_level);
            }
            Node::StructExpression(struct_expr) => {
                let TokenEnum::Identifier(name) = &struct_expr.name.kind else {unreachable!()};
                self.add_with_indent(&format!("{}: (\n", name), indent_level);
                for field in &struct_expr.fields {
                    let TokenEnum::Identifier(field_name) = &field.field_name.kind else {unreachable!()};
                    let type_code = Self::get_type_expression_code(&field.field_type, &[]);
                    self.add_with_indent(&format!("{} :{}\n", field_name, type_code), indent_level + 1);
                }
                self.add_with_indent(") datadef\n\n", indent_level);
            }
            Node::EnumExpression(enum_expr) => {
                let TokenEnum::Identifier(name) = &enum_expr.name.kind else {unreachable!()};
                self.add_with_indent(&format!("{}: {{\n", name), indent_level);
                for variant in &enum_expr.variants {
                    let TokenEnum::Identifier(variant_name) = &variant.variant_name.kind else {unreachable!()};
                    let mut variant_name = variant_name.clone();
                    if variant.inner.is_some() {
                        variant_name += "_"; // We need to do this because otherwise the inner type might conflict with already created types (I LOVE Postfix)
                    }
                    let type_code = variant.inner.as_ref()
                        .map(|inner| {
                            let code = Self::get_type_expression_code(inner, &[]);
                            format!("inner :{}", code)
                        })
                        .unwrap_or("".to_string());
                    self.add_with_indent(&format!("{}: ({})\n", variant_name, type_code), indent_level + 1);
                }
                self.add_with_indent("} datadef\n\n", indent_level);
            }
            Node::WhileExpression(WhileExpression {condition, body , ..}) => {
                // Modify body ast to include breaking when condition is met (calling the breakif function)
                let mut modified_body = *body.clone();
                let Node::ExpressionList(ExpressionList {expressions, ..}) = &mut modified_body else {unreachable!("A while expression always has a expression list body")};
                expressions.insert(0, Node::CallExpression(CallExpression {
                    name: Token { kind: TokenEnum::Identifier("breakif".to_string()), span: Default::default() },
                    closing_parenthesis: Token { kind: TokenEnum::NoToken, span: Default::default() },
                    arguments: vec![Node::UnaryExpression(UnaryExpression {
                        operation: Token { kind: TokenEnum::Not, span: Default::default() },
                        expression: condition.clone()
                    })],
                }));
                self.generate_code(&modified_body, indent_level);
                self.add_with_indent(" loop", indent_level);
            }
            Node::ForExpression(ForExpression {iteration_var, iterate_over, body , ..}) => {
                let iteration_var = match &iteration_var.kind {
                    TokenEnum::Identifier(var) => Some(var),
                    TokenEnum::Underscore => None,
                    _ => unreachable!()
                };
                // Modify body ast to include binding iteration value to variable (or drop the variable with '_')
                let mut modified_body = *body.clone();
                let Node::ExpressionList(ExpressionList {expressions, ..}) = &mut modified_body else {unreachable!("A while expression always has a expression list body")};
                if let Some(var) = iteration_var {
                    expressions.insert(0, Node::_Verbatim(format!(" {}!", var)));
                } else {
                    expressions.insert(0, Node::_Verbatim(" pop".to_string()));
                }
                self.generate_code(iterate_over, indent_level);
                self.generate_code(&modified_body, indent_level);
                self.add_with_indent(" for", indent_level);
            }
            Node::ReturnExpression(ReturnExpression {expression, ..}) => self.generate_code(expression, indent_level),
            Node::BreakExpression(BreakExpression {..}) => self.add_with_indent("break", indent_level),
            Node::IndexExpression(IndexExpression { index_value, index_into, ..}) => {
                self.generate_code(index_into, indent_level);
                self.add_with_indent(" ", indent_level);
                self.generate_code(index_value, indent_level);
                self.add_with_indent(" get", indent_level);
            }
            Node::DotChainExpression(DotChainExpression {expressions}) => {
                for expression in expressions {
                    self.generate_code(expression, indent_level);
                    self.add_with_indent(" ", indent_level);
                }
                self.code.pop(); // Remove trailing " "
            },
            Node::CommentExpression(CommentExpression {comment, on}) => {
                if let Some(on) = on {
                    self.generate_code(on, indent_level);
                }
                let TokenEnum::Comment(comment_string) = &comment.kind else {unreachable!()};
                self.add_with_indent(&format!(" #{}{}", comment_string, if on.is_some() {"\n"} else {""}), indent_level);
            },
            Node::_Verbatim(code) => {
                self.add_with_indent(code, indent_level);
            }
        }
    }

    pub fn new(dot_chain_access_types: DotChainAccessTypes, structs: HashMap<String, Struct>) -> Self {
        Self {
            code: Default::default(),
            dot_chain_access_types,
            structs
        }
    }
}


fn replace_function_ident(ident_str: &str) -> String {
    FUNCTION_REPLACEMENTS.get(ident_str)
        .map(|s| s.to_string())
        .unwrap_or_else(|| ident_str.replace('_', "-"))
}
