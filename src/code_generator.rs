use crate::node::{Node, prelude::*};
use crate::r#type::Type;
use crate::token::TokenEnum;
use crate::type_checker::DotChainAccessTypes;

const INDENT: &'static str = "    ";
const MAX_LINE_LENGTH: usize = 60;

const FUNCTION_REPLACEMENTS: [(&'static str, &'static str); 6] = [
    ("test_eq", "test="),
    ("test_neq", "test!="),
    ("char_to_str", "char->str"),
    ("chars_to_str", "chars->str"),
    ("str_to_chars", "str->chars"),
    ("_", "-"),
];

#[derive(Default)]
pub struct CodeGenerator {
    pub code: String,
    dot_chain_access_types: DotChainAccessTypes
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

    fn get_type_expression_code<'a>(type_expression: &'a TypeExpression, generic_types: &Vec<&str>) -> &'a str {
        match type_expression {
            TypeExpression::SingleTypeExpression(SingleTypeExpression { type_name: type_name_token, .. }) => {
                let TokenEnum::Identifier(type_name) = &type_name_token.kind else {unreachable!()};
                if generic_types.contains(&type_name.as_str()) {"Obj"} else {type_name}
            }
            TypeExpression::UnionTypeExpression(_) => "Obj"
        }
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
            Node::DotChainAccess(DotChainAccess { ident, access_id }) => {
                let TokenEnum::Identifier(property_name) = &ident.kind else {unreachable!()};
                let Type::Struct(struct_name) = self.dot_chain_access_types.get(access_id).unwrap() else {unimplemented!("DotChainAccess is only implemented for structs")};
                let access_function_name = format!("{}-{}", struct_name.to_lowercase(), property_name);
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
                for pair in pairs {
                    // TODO: This is not correct as the order in the instantiation might not be the same as in the definition. To account for this we need the "structs" data from the type checker
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
            Node::AssignmentExpression(AssignmentExpression {to, value, ..}) => {
                if let Node::IndexExpression(IndexExpression {index_value, index_into, ..}) = &**to {
                    // This is required as a normally generating the code for a index expression would modify a copy of the gotten value, which is unwanted and might be invalid
                    let mut value_path_expressions = vec![index_value];
                    let mut current_expression = index_into;
                    while let Node::IndexExpression(IndexExpression { index_value: inner_index_value, index_into: inner_index_into, ..}) = current_expression.as_ref() {
                        value_path_expressions.push(inner_index_value);
                        current_expression = inner_index_into;
                    }
                    if let Node::IdentifierExpression(_) = current_expression.as_ref() {} else { panic!("First index into always has to be a identifier expression") }
                    self.generate_code(current_expression, indent_level);
                    self.add_with_indent(" ", indent_level);
                    // Use set with simple index or path-set with array of indices for nested index expressions
                    // We cannot use path-set for everything, as it only works on arrays while set also works on strings
                    let function_name = if value_path_expressions.len() == 1 {
                        self.generate_code(index_value, indent_level);
                        "set"
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
                        self.generate_code(&path_array_expression_list, indent_level);
                        "path-set"
                    };
                    self.add_with_indent(" ", indent_level);
                    self.generate_code(value, indent_level);
                    self.add_with_indent(&format!(" {} ", function_name), indent_level);
                    self.generate_code(current_expression, indent_level);
                } else {
                    self.generate_code(value, indent_level);
                    self.add_with_indent(" ", indent_level);
                    self.generate_code(to, indent_level);
                }
                self.add_with_indent("!", indent_level);
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
                    let type_code = Self::get_type_expression_code(&field.field_type, &vec![]);
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
                            let code = Self::get_type_expression_code(inner, &vec![]);
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

    pub fn new(dot_chain_access_types: DotChainAccessTypes) -> Self {
        Self {
            code: Default::default(),
            dot_chain_access_types,
        }
    }
}


fn replace_function_ident(ident_str: &str) -> String {
    let mut out = ident_str.replace(FUNCTION_REPLACEMENTS[0].0, FUNCTION_REPLACEMENTS[0].1);
    for (x, with) in FUNCTION_REPLACEMENTS {
        out = out.replace(x, with);
    }
    out
}
