use crate::node::{Node, prelude::*};
use crate::token::TokenEnum;

const INDENT: &'static str = "    ";
const MAX_LINE_LENGTH: usize = 60;

const FUNCTION_REPLACEMENTS: [(&'static str, &'static str); 3] = [
    ("assert_eq", "test="),
    ("assert_neq", "test!="),
    ("_", "-"),
];

#[derive(Default)]
pub struct CodeGenerator {
    pub code: String
}


impl CodeGenerator {
    fn add_with_indent(&mut self, mut add: &str, mut indent_level: usize) {
        if add.is_empty() {return;}
        if !self.code.ends_with("\n") {
            indent_level = 0;
        } else if !self.code.ends_with(" ") {
            add = add.trim_start();
        }
        let indent_level = (indent_level as isize - 1).max(0) as usize;
        self.code += &(INDENT.repeat(indent_level) + add);
        // Add auto line break after hitting MAX_LINE_LENGTH
        let current_line_length = self.code.chars()
            .rev()
            .take_while(|c| c != &'\n')
            .count();
        if current_line_length > MAX_LINE_LENGTH && !add.ends_with("\n") {
            self.code += "\n";
        }
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
                    TokenEnum::FloatLiteral(float) => format!("{}", float),
                    TokenEnum::BooleanLiteral(boolean) => format!("{}", boolean),
                    TokenEnum::StringLiteral(string) => format!("\"{}\"", string),
                    _ => unreachable!()
                };
                self.add_with_indent(&literal, indent_level);
            }
            Node::IdentifierExpression(token) => {
                let TokenEnum::Identifier(ident) = &token.kind else {unreachable!()};
                self.add_with_indent(ident, indent_level);
            }
            Node::CallExpression(CallExpression { name, arguments, .. }) => {
                for argument in arguments {
                    self.generate_code(argument, indent_level);
                    self.add_with_indent(" ", indent_level);
                }
                let TokenEnum::Identifier(ident) = &name.kind else {unreachable!()};
                self.add_with_indent(&replace_function_ident(ident), indent_level);
            }
            Node::ParenthesizedExpression(node) => self.generate_code(node, indent_level),
            Node::AssignmentExpression(AssignmentExpression {to, value, ..}) => {
                self.generate_code(value, indent_level);
                self.add_with_indent(" ", indent_level);
                self.generate_code(to, indent_level);
                self.add_with_indent("!", indent_level);
            },
            Node::ExpressionList(ExpressionList {expressions, ..}) => {
                if indent_level > 0 {
                    self.add_with_indent("{\n", indent_level);
                }
                for expression in expressions {
                    self.generate_code(expression, indent_level + 1);
                    self.add_with_indent("\n", indent_level);
                }
                if indent_level > 0 {
                    self.add_with_indent("}", indent_level);
                }
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
            Node::FunctionExpression(FunctionExpression { name, parameters, return_type, body, .. }) => {
                let TokenEnum::Identifier(name_ident) = &name.kind else {unreachable!()};
                let name_ident = replace_function_ident(name_ident);
                self.add_with_indent(&format!(":{}(", name_ident), indent_level);
                for parameter in parameters {
                    let TokenEnum::Identifier(parameter_name) = &parameter.name.kind else {unreachable!()};
                    let TokenEnum::Identifier(parameter_type) = &parameter.parameter_type.kind else {unreachable!()};
                    self.add_with_indent(&format!("{} :{},", parameter_name, parameter_type), indent_level);
                }
                self.code.pop(); // Removes unnecessary trailing ','
                if let Some(return_type) = return_type {
                    let TokenEnum::Identifier(return_type_ident) = &return_type.return_type.kind else {unreachable!()};
                    self.add_with_indent(&format!(" -> :{}", return_type_ident), indent_level);
                };
                self.add_with_indent(") ", indent_level);
                self.generate_code(body, indent_level);
                self.add_with_indent(" fun\n\n", indent_level);
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
                self.generate_code(&iterate_over, indent_level);
                self.generate_code(&modified_body, indent_level);
                self.add_with_indent(" for", indent_level);
            }
            Node::ReturnExpression(ReturnExpression {expression, ..}) => self.generate_code(expression, indent_level),
            Node::BreakExpression(BreakExpression {..}) => self.add_with_indent("break", indent_level),
            Node::IndexExpression(IndexExpression { index_value, index_into, ..}) => {
                self.generate_code(index_value, indent_level);
                self.add_with_indent(" ", indent_level);
                self.generate_code(index_into, indent_level);
                self.add_with_indent(" get", indent_level);
            }
            Node::CommentExpression(CommentExpression {comment, on}) => {
                if let Some(on) = on {
                    self.generate_code(on, indent_level);
                }
                let TokenEnum::Comment(comment_string) = &comment.kind else {unreachable!()};
                self.add_with_indent(&format!(" #{}{}", comment_string, if on.is_some() {"\n"} else {""}), indent_level);
            },
            Node::_Verbatim(code) => {
                self.add_with_indent(&code, indent_level);
            }
        }
    }
}


fn replace_function_ident(ident_str: &str) -> String {
    let mut out = ident_str.replace(FUNCTION_REPLACEMENTS[0].0, FUNCTION_REPLACEMENTS[0].1);
    for i in 1..FUNCTION_REPLACEMENTS.len() {
        let (x, with) = FUNCTION_REPLACEMENTS[i];
        out = out.replace(x, with);
    }
    out
}
