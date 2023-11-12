use std::collections::HashMap;
use bincode::{Decode, Encode};
use crate::r#type::{GenericPath, Type};

#[derive(Debug, Clone, Encode, Decode)]
pub struct Function {
    pub parameters: Vec<(String, Type)>,
    pub generic_parameter_map: HashMap<String, GenericPath>, // Maps a name of a generic type to a path towards that generic in the parameters
    pub returns: Type,
    pub scope_id: u64,
}

#[derive(Debug, Clone, Default, Encode, Decode)]
pub struct Scope {
    pub functions: HashMap<String, Function>,
    pub variables: HashMap<String, Type>,
    pub parent_scope : Option<u64>
}

impl Scope {
    pub fn new(parent_scope_id : Option<u64>) -> Self {
        Self {
            functions: Default::default(),
            variables: Default::default(),
            parent_scope: parent_scope_id
        }
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
    pub(crate) fn find_variable_scope_id_in_scope_by_name(&self, scope_id : u64, variable_name: &str) -> Option<u64> {
        // Try to find variable value going up from the current scope
        let mut current_scope_id = Some(scope_id);
        let mut found_variable = false;
        while current_scope_id.is_some() {
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
    pub fn find_function_scope_id_in_scope_by_name(&self, scope_id : u64, function_name: &str) -> Option<u64> {
        // Try to find function value going up from the current scope
        let mut current_scope_id = Some(scope_id);
        let mut found_function = false;
        while current_scope_id.is_some() {
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
    pub(crate) fn find_variable_in_scope_by_name(&self, scope_id : u64, variable_name: &str) -> Option<&Type> {
        match self.find_variable_scope_id_in_scope_by_name(scope_id, variable_name) {
            Some(scope_id) => {
                self.get(&scope_id).variables.get(variable_name)
            },
            None => None
        }
    }

}