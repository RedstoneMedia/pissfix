use std::collections::HashMap;
use crate::errors::{Error, ErrorKind};
use crate::GetSpan;
use crate::node::Node;
use crate::token::{Token, TokenEnum};

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub(crate) enum Type {
    Integer,
    Float,
    String,
    Boolean,
    Object,
    Range,
    Lambda(Box<Function>),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Union(Vec<Type>),
    #[default]
    _Unknown, // Only internally used
    _None  // Only internally used
}

impl Type {

    pub(crate) fn union_from(a: Type, b: Type) -> Type {
        if a == b {return a}
        let mut types = Vec::new();
        if let Type::Union(mut subtypes) = a {
            types.append(&mut subtypes);
        } else {
            types.push(a);
        }
        if let Type::Union(mut subtypes) = b {
            types.append(&mut subtypes);
        } else {
            types.push(b)
        }
        Type::Union(types)
    }


    pub(crate) fn expect_type(actual: &Type, expected: &Type) -> bool {
        if actual == expected || actual == &Type::_Unknown || expected == &Type::_Unknown {return true};
        let mut types_actual = Vec::new();
        if let Type::Union(subtypes) = actual {
            types_actual.extend(subtypes.iter());
        } else {
            types_actual.push(actual)
        }
        let mut types_expected = Vec::new();
        if let Type::Union(subtypes) = expected {
            types_expected.extend(subtypes.iter());
        } else {
            types_expected.push(expected)
        }

        let mut does_overlap = false;
        for t in types_actual {
            if types_expected.contains(&t) {
                does_overlap = true;
                break;
            }
        }

        return does_overlap
    }

}

impl TryFrom<&Token> for Type {
    type Error = Error;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match &value.kind {
            TokenEnum::Identifier(s) => match s.as_ref() {
                "Int" => Ok(Type::Integer),
                "Flt" => Ok(Type::Float),
                "Str" => Ok(Type::String),
                "Bool" => Ok(Type::Boolean),
                "Obj" => Ok(Type::Object),
                "Lam" => Ok(Type::Lambda(Box::new(Function {
                    parameters: vec![], // Unknown until generics are implemented
                    returns: Type::_Unknown, // Unknown until generics are implemented
                    scope_id: u64::MAX, // Unknown until generics are implemented
                }))),
                "Arr" => Ok(Type::Array(Box::new(Type::_Unknown))), // Unknown until generics are implemented
                _ => Err(Error::from_span(
                    value.get_span(),
                    format!("Invalid type name: \"{}\"", s),
                    ErrorKind::TypeCheckError
                ))
            },
            _ => Err(Error::from_span(
                value.get_span(),
                format!("Invalid token type for Type: {:?}", value),
                ErrorKind::TypeCheckError
            ))
        }
    }
}

impl TryFrom<&Node> for Type {

    type Error = Error;

    fn try_from(value: &Node) -> Result<Self, Self::Error> {
        match value {
            Node::IdentifierExpression(ident) => Type::try_from(ident),
            _ => Err(Error::from_span(
                value.get_span(),
                format!("Invalid node for Type: {:?}", value),
                ErrorKind::TypeCheckError
            ))
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate)struct Function {
    pub parameters: Vec<(String, Type)>,
    pub returns: Type,
    pub scope_id: u64,
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub(crate) struct Scope {
    pub functions: HashMap<String, Function>,
    pub variables: HashMap<String, Type>,
    pub parent_scope : Option<u64>
}

impl Scope {
    pub fn new(parent_scope_id : Option<u64>) -> Self {
        Self { functions: Default::default(), variables: Default::default(), parent_scope: parent_scope_id }
    }
}

#[derive(Debug, Default)]
pub struct AllScopes {
    scopes : HashMap<u64, Scope>,
    count : u64
}

impl AllScopes {

    pub(crate) fn insert(&mut self, scope : Scope) -> u64 {
        let new_index = self.count;
        self.scopes.insert(new_index, scope);
        self.count += 1;
        new_index
    }

    pub(crate) fn get_mut(&mut self, index : &u64) -> &mut Scope {
        self.scopes.get_mut(index).expect(&format!("Scope does not exist with id : {}", index))
    }

    pub(crate) fn get(&self, index : &u64) -> &Scope {
        self.scopes.get(index).expect(&format!("Scope does not exist with id : {}", index))
    }

    /// This function tries to find a input variable by name in or above the input scope.
    /// This function will return None, if the variable is not found,
    /// or the lowest possible scope in which the variable exists.
    pub(crate) fn find_variable_scope_id_in_scope_by_name(&self, scope_id : u64, variable_name: &String) -> Option<u64> {
        // Try to find variable value going up from the current scope
        let mut current_scope_id = Some(scope_id);
        let mut found_variable = false;
        while current_scope_id.is_some() {
            if current_scope_id.unwrap() == 0 {break;}
            let scope = self.get(&current_scope_id.unwrap());
            let parent_scope_id = scope.parent_scope;
            if scope.variables.contains_key(variable_name) {
                found_variable = true;
                break;
            }
            current_scope_id = parent_scope_id;
        }
        if found_variable { current_scope_id } else { None }
    }

    /// This function does the same thing as find_variable_scope_id_in_scope_by_name, but searches for a function and not variable.
    pub fn find_function_scope_id_in_scope_by_name(&self, scope_id : u64, function_name: &String) -> Option<u64> {
        // TODO: This does not seem to be working
        // Try to find function value going up from the current scope
        let mut current_scope_id = Some(scope_id);
        let mut found_function = false;
        while current_scope_id.is_some() {
            if current_scope_id.unwrap() == 0 {break;}
            let scope = self.get(&current_scope_id.unwrap());
            let parent_scope_id = scope.parent_scope;
            if scope.functions.contains_key(function_name) {
                found_function = true;
                break;
            }
            current_scope_id = parent_scope_id;
        }
        if found_function { current_scope_id } else { None }
    }

    /// This function tries to find a variable by name and returns a immutable reference to that variable if it found it.
    pub(crate) fn find_variable_in_scope_by_name(&self, scope_id : u64, variable_name: &String) -> Option<&Type> {
        match self.find_variable_scope_id_in_scope_by_name(scope_id, &variable_name) {
            Some(scope_id) => {
                self.get(&scope_id).variables.get(variable_name)
            },
            None => None
        }
    }

}