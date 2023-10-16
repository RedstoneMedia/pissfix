use std::collections::HashMap;
use std::usize;
use crate::scope::Function;

/// Holds all types that were Unknown at one point All references
/// A reference type in here can *NEVER* be deleted
#[derive(Debug, Clone, Default)]
pub(crate) struct AllReferences {
    types: Vec<Type>
}

impl AllReferences {

    pub(crate) fn insert(&mut self, t: Type) -> usize {
        self.types.push(t);
        self.types.len() - 1
    }

    pub(crate) fn replace(&mut self, id: usize, new: Type) {
        self.types[id] = new;
    }

    pub(crate) fn get_mut(&mut self, id: &usize) -> &mut Type {
        self.types.get_mut(*id).expect(&format!("Reference does not exist with id : {}", id))
    }

    pub(crate) fn get(&self, id: &usize) -> &Type {
        self.types.get(*id).expect(&format!("Reference does not exist with id : {}", id))
    }

}


#[derive(Debug, Clone, Default)]
pub(crate) struct Generic {
    pub name: String,
    pub base: GenericBase
}

#[derive(Debug, Clone)]
pub(crate) enum GenericBase {
    Requirements(TypeRequirements),
    Specific(Type)
}

impl Default for GenericBase {
    fn default() -> Self {
        Self::Requirements(Default::default())
    }
}


#[derive(Debug, Clone, Default)]
pub(crate) struct TypeRequirements {
    pub requirements: Vec<TypeRequirement>
}

impl TypeRequirements {

    pub(crate) fn new(requirements: Option<Vec<TypeRequirement>>, all_unknown: &mut AllReferences) -> Type {
        let id = all_unknown.insert(Type::Unknown(Box::new(Self {
            requirements: requirements.unwrap_or_else(|| Default::default()),
        })));
        Type::Reference(id)
    }

    fn ensure_fulfills(&mut self, t: &Type, all_references: &mut AllReferences, in_fn_call: bool) -> bool {
        if let Type::Reference(additional_requirements_id) = t {
            if let Some(additional_requirements) = all_references.get(additional_requirements_id).requirements().cloned() {
                for requirement in additional_requirements.requirements {
                    self.require(requirement, all_references, in_fn_call);
                }
                return true; // Generics are always fulfilled, if the requirements are made to match
            }
        }
        self.requirements.iter()
            .all(|req| {
                req.does_fulfill(t, all_references, in_fn_call)
            })
    }

    fn require(&mut self, mut requirement: TypeRequirement, all_references: &mut AllReferences, in_fn_call: bool) {
        let already_required = self.requirements.iter()
            .any(|req| req.eq(&requirement, all_references, in_fn_call));
        if !already_required {
            // Always create inner generic for Index requirement
            if let TypeRequirement::Index(None) = requirement {
                let inner_id = all_references.insert(Type::Unknown(Default::default()));
                requirement = TypeRequirement::Index(Some(inner_id))
            }
            self.requirements.push(requirement);
        }
    }

    pub(crate) fn to_string(&self, all_references: &AllReferences) -> String {
        if self.requirements.is_empty() {
            return "Type".to_string();
        }
        let requirements_strings : Vec<_> = self.requirements.iter()
            .map(|req| req.to_string(all_references))
            .collect();
        let requirement_string = requirements_strings.join(" and ");
        if requirements_strings.len() > 1 {
            format!("Type supporting: ({})", requirement_string)
        } else {
            format!("Type supporting {}", requirement_string)
        }
    }

}


impl Generic {

    pub(crate) fn new_requirement_based(name: String, requirements: Option<TypeRequirements>, all_references: &mut AllReferences) -> Type {
        let id = all_references.insert(Type::Generic(Box::new(Self {
            name,
            base: GenericBase::Requirements(requirements.unwrap_or_else(|| Default::default())),
        })));
        Type::Reference(id)
    }

    pub(crate) fn new_specific_based(name: String, specific_type: Type) -> Type {
        Type::Generic(Box::new(Self {
            name,
            base: GenericBase::Specific(specific_type),
        }))
    }

    pub(crate) fn to_string(&self, all_references: &AllReferences) -> String {
        let name = if self.name.is_empty() {
            "".to_string()
        } else {
            format!(" {}", self.name)
        };
        match &self.base {
            GenericBase::Requirements(requirements) => format!("Generic{} {}", name, requirements.to_string(all_references)),
            GenericBase::Specific(t) => t.to_string(all_references)
        }

    }

    fn ensure_fulfills(&mut self, t: &Type, all_references: &mut AllReferences, in_fn_call: bool) -> bool {
        match &mut self.base {
            GenericBase::Requirements(requirements) => requirements.ensure_fulfills(t, all_references, in_fn_call),
            GenericBase::Specific(specific_type) => t.expect_to_be(specific_type, all_references, in_fn_call)
        }
    }

    fn require(&mut self, mut requirement: TypeRequirement, all_references: &mut AllReferences, in_fn_call: bool) {
        match &mut self.base {
            GenericBase::Requirements(requirements) => {
                let already_required = requirements.requirements.iter()
                    .any(|req| req.eq(&requirement, all_references, in_fn_call));
                if !already_required {
                    // Always create inner generic for Index requirement
                    if let TypeRequirement::Index(None) = requirement {
                        let inner_id = all_references.insert(Type::Generic(Default::default()));
                        requirement = TypeRequirement::Index(Some(inner_id))
                    }
                    requirements.requirements.push(requirement);
                }
            }
            GenericBase::Specific(_) => unimplemented!()
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum TypeRequirement {
    Iteration(Option<usize>),
    Index(Option<usize>),
    Equality(Type),
    Addition(Type),
    Subtraction(Type),
    Multiplication(Type),
    Division(Type),
    Ord(Type),
    Negation
}

impl TypeRequirement {

    pub fn does_fulfill(&self, t: &Type, all_references: &mut AllReferences, in_fn_call: bool) -> bool {
        let r = match (self, t) {
            (_, Type::Reference(id)) => {
                let mut ref_type = Default::default();
                std::mem::swap(&mut ref_type, all_references.get_mut(id));

                let r = match &mut ref_type {
                    Type::Generic(generic) => {
                        generic.require(self.clone(), all_references, in_fn_call);
                        true
                    },
                    Type::Unknown(requirements) => {
                        requirements.require(self.clone(), all_references, in_fn_call);
                        true
                    }
                    _ => {
                        self.does_fulfill(&ref_type, all_references, in_fn_call)
                    }
                };
                std::mem::swap(&mut ref_type, all_references.get_mut(id));
                r
            },
            (_, Type::Union(types)) => {
                types.iter().all(|t| self.does_fulfill(t, all_references, in_fn_call))
            },
            (Self::Index(None) | Self::Iteration(None), Type::Array(_)) => true,
            (Self::Index(Some(inner_ref_id)) | Self::Iteration(Some(inner_ref_id)), Type::Array(inner)) => {
                let mut ref_type = Default::default();
                std::mem::swap(&mut ref_type, all_references.get_mut(inner_ref_id));
                let r = if let Type::Generic(inner_generic) = &mut ref_type {
                    inner_generic.ensure_fulfills(&Type::Reference(*inner), all_references, in_fn_call)
                } else {
                    ref_type.expect_to_be(&Type::Reference(*inner), all_references, in_fn_call)
                };
                std::mem::swap(&mut ref_type, all_references.get_mut(inner_ref_id));
                r
            }
            (Self::Index(None) | Self::Iteration(None), Type::String) => true,
            (Self::Index(Some(inner_ref_id)) | Self::Iteration(Some(inner_ref_id)), Type::String) => {
                Type::Reference(*inner_ref_id).expect_to_be(&Type::String, all_references, in_fn_call) // TODO: Maybe change to Char type (when that exists)
            }
            // Ranges can not be index only iterated over
            (Self::Iteration(None), Type::Range) => true,
            (Self::Iteration(Some(inner_ref_id)), Type::Range) => {
                Type::Reference(*inner_ref_id).expect_to_be(&Type::Integer, all_references, in_fn_call)
            },
            (Self::Negation, Type::Integer | Type::Float) => true,
            (
                Self::Equality(with)
                | Self::Addition(with)
                | Self::Subtraction(with)
                | Self::Multiplication(with)
                | Self::Division(with)
                | Self::Ord(with)
                , _
            ) => {
                t.expect_to_be(with, all_references, in_fn_call)
            },
            _ => false
        };
        r
    }

    fn eq(&self, other: &Self, all_references: &mut AllReferences, in_fn_call: bool) -> bool {
        if let (TypeRequirement::Index(id_a), TypeRequirement::Index(id_b)) = (self, other) {
            if id_a.is_none() || id_b.is_none() {
                return true;
            }
            return id_a == id_b;
        }
        let (with_a, with_b) = match (self, other) {
            (TypeRequirement::Equality(a), TypeRequirement::Equality(b)) => (a, b),
            (TypeRequirement::Addition(a), TypeRequirement::Addition(b)) => (a, b),
            (TypeRequirement::Subtraction(a), TypeRequirement::Subtraction(b)) => (a, b),
            (TypeRequirement::Multiplication(a), TypeRequirement::Multiplication(b)) => (a, b),
            (TypeRequirement::Division(a), TypeRequirement::Division(b)) => (a, b),
            (_, _) => return false
        };
        with_a.expect_to_be(with_b, all_references, in_fn_call)
    }

    pub(crate) fn to_string(&self, all_references: &AllReferences) -> String {
        match self {
            TypeRequirement::Index(id) | TypeRequirement::Iteration(id) => {
                let name = match self {
                    TypeRequirement::Index(_) => "Indexing",
                    TypeRequirement::Iteration(_) => "Iteration",
                    _ => unreachable!()
                };
                if let Some(id) = id {
                    let generic = all_references.get(id);
                    format!("{} with inner: [{}]", name, generic.to_string(all_references))
                } else {
                    name.to_string()
                }
            },
            TypeRequirement::Equality(with) => format!("Equality with {}", with.to_string(all_references)),
            TypeRequirement::Addition(with) => format!("Addition with {}", with.to_string(all_references)),
            TypeRequirement::Subtraction(with) => format!("Subtraction with {}", with.to_string(all_references)),
            TypeRequirement::Multiplication(with) => format!("Multiplication with {}", with.to_string(all_references)),
            TypeRequirement::Division(with) => format!("Division with {}", with.to_string(all_references)),
            TypeRequirement::Ord(with) => format!("Order with {}", with.to_string(all_references)),
            TypeRequirement::Negation => "Negation".to_string(),
        }
    }

}

#[derive(Debug, Clone, Default)]
pub(crate) enum Type {
    Integer,
    Float,
    String,
    Boolean,
    Range,
    Lambda(Box<Function>),
    Tuple(Vec<Type>),
    Array(usize),
    Any, // Acts like a union of all types
    Union(Vec<Type>),
    Reference(usize),
    // These types should *always* be behind a Reference Type
    Generic(Box<Generic>),
    Unknown(Box<TypeRequirements>),
    #[default]
    _None  // Only internally used
}

impl Type {

    pub(crate) fn try_into_iter_inner(&self, all_references: &AllReferences) -> Option<Self> {
        match self {
            Type::String => Some(Type::String), // Or Char maybe at some point
            Type::Reference(id) => {
                let ref_type = all_references.get(id);
                ref_type.try_into_iter_inner(all_references)
            }
            Type::Generic(_) | Type::Unknown(_) => {
                let requirements = self.requirements().unwrap();
                requirements.requirements.iter()
                    .find_map(|req| if let TypeRequirement::Index(inner_ref_type) = req {*inner_ref_type} else {None})
                    .map(Type::Reference)
            },
            Type::Range => Some(Type::Integer),
            Type::Tuple(types) => unimplemented!(),
            Type::Array(inner_id) => Some(Type::Reference(*inner_id)),
            _ => None
        }
    }

    fn ref_expect_to_be(&self, expected_type_id: &usize, all_references: &mut AllReferences, in_fn_call: bool) -> bool {
        let mut ref_type = Default::default();
        std::mem::swap(&mut ref_type, all_references.get_mut(expected_type_id));
        let r = match (self, &mut ref_type) {
            (Type::Reference(actual_ref_id), Type::Generic(generic)) => {
                assert_ne!(actual_ref_id, expected_type_id);
                let actual_ref_type = all_references.get(actual_ref_id);
                match actual_ref_type {
                    Type::Generic(_) | Type::Unknown(_) => generic.ensure_fulfills(self, all_references, in_fn_call),
                    _ if in_fn_call => generic.ensure_fulfills(self, all_references, in_fn_call),
                    _ => false,
                }
            },
            (Type::Reference(actual_ref_id), Type::Unknown(requirements)) => {
                assert_ne!(actual_ref_id, expected_type_id);
                let actual_ref_type = all_references.get(actual_ref_id);
                match actual_ref_type {
                    Type::Unknown(_) | Type::Generic(_) => requirements.ensure_fulfills(self, all_references, in_fn_call),
                    _ => {
                        let r = requirements.ensure_fulfills(self, all_references, in_fn_call); // Different from generics: Expect to be is legal not only when the other type is a unknown
                        ref_type = self.clone(); // Unknown gets collapsed to real type
                        r
                    }
                }
            },
            (_, Type::Generic(generic)) => {
                if in_fn_call {
                    generic.ensure_fulfills(self, all_references, in_fn_call)
                } else {
                    false
                }
            }
            (_, Type::Unknown(requirements)) => {
                let r = requirements.ensure_fulfills(self, all_references, in_fn_call);
                ref_type = self.clone();
                r
            }
            (_, _) => {
                self.expect_to_be(&ref_type, all_references, in_fn_call)
            }
        };
        std::mem::swap(&mut ref_type, all_references.get_mut(expected_type_id));
        r
    }

    pub(crate) fn expect_to_be(&self, expected: &Type, all_references: &mut AllReferences, in_fn_call: bool) -> bool {
        match (expected, self) {
            (Type::Reference(r_id), Type::Reference(a_id)) => {
                if r_id == a_id {
                    return true;
                }
                self.ref_expect_to_be(r_id, all_references, in_fn_call) //&& expected.ref_expect_to_be(a_id, all_references, in_fn_call)
            },
            (Type::Reference(r_id), _) => self.ref_expect_to_be(r_id, all_references, in_fn_call),
            (_, Type::Reference(a_id)) => expected.ref_expect_to_be(a_id, all_references, in_fn_call), // Does not work because expected becomes actual here
            (Type::Generic(gen), _) => {
                match &gen.base {
                    GenericBase::Requirements(_) => unreachable!("Generic requirements are always behind reference"),
                    GenericBase::Specific(t) => self.expect_to_be(&t, all_references, in_fn_call)
                }
            }
            (_, Type::Generic(gen)) => { // e a
                match &gen.base {
                    GenericBase::Requirements(_) => unreachable!("Generic requirements are always behind reference"),
                    GenericBase::Specific(t) => t.expect_to_be(expected, all_references, in_fn_call)
                }
            }
            (Type::Any, _) | (_, Type::Any) if in_fn_call => true,
            (Type::Any, Type::Any) => true,
            (Type::Integer, Type::Integer) => true,
            (Type::Float, Type::Float) => true,
            (Type::String, Type::String) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Range, Type::Range) => true,
            (Type::_None, Type::_None) => true,
            (Type::Lambda(expected_fun),  Type::Lambda(actual_fun)) => {
                // TODO: Implement
                true
            },
            (Type::Tuple(expected_types), Type::Tuple(actual_types)) => {
                for (sub_expected, sub_actual) in expected_types.iter().zip(actual_types) {
                    if !sub_actual.expect_to_be(sub_expected, all_references, in_fn_call) {
                        return false;
                    }
                }
                true
            }
            (Type::Tuple(expected_types), actual) => {
                if expected_types.len() == 1 {
                    actual.expect_to_be(&expected_types[0], all_references, in_fn_call)
                } else {
                    false
                }
            }
            (Type::Array(inner_expected), Type::Array(inner_actual)) => {
                Type::Reference(*inner_actual).expect_to_be(&Type::Reference(*inner_expected), all_references, in_fn_call)
            }
            (expected, Type::Union(types_actual)) => {
                let mut types_expected = Vec::new();
                if let Type::Union(subtypes) = expected {
                    types_expected.extend(subtypes.iter());
                } else {
                    types_expected.push(expected)
                }
                // Actual Union has to be of same or higher specificity
                if types_expected.len() < types_actual.len() {
                    return false;
                }
                // Check if all types overlap
                types_actual
                    .iter()
                    .all(|sub_actual| types_expected
                        .iter()
                        .any(|sub_expected| sub_actual.expect_to_be(sub_expected, all_references, in_fn_call))
                    )
            },
            (Type::Union(types_expected), actual) => {
                types_expected
                    .iter()
                    .any(|sub_expected| actual.expect_to_be(sub_expected, all_references, in_fn_call))
            },
            (_, _) => false
        }
    }


    pub(crate) fn require(&self, requirement: TypeRequirement, all_generics: &mut AllReferences, in_fn_call: bool) -> Result<(), ()> {
        if !requirement.does_fulfill(self, all_generics, in_fn_call) {
            return Err(());
        }
        if let Type::Reference(id) = self {
            let mut generic_type = Default::default();
            std::mem::swap(&mut generic_type, all_generics.get_mut(id));
            if let Type::Generic(generic) = &mut generic_type {
                generic.require(requirement, all_generics, in_fn_call);
            }
            std::mem::swap(&mut generic_type, all_generics.get_mut(id));
        }
        Ok(())
    }

    pub(crate) fn to_string(&self, all_references: &AllReferences) -> String {
        match self {
            Type::Integer => "Integer".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Boolean => "Boolean".to_string(),
            Type::Range => "Range".to_string(),
            Type::Any => "Any".to_string(),
            Type::Lambda(func) => format!(
                "Lambda ({}) -> {}",
                func.parameters.iter()
                    .map(|(_, t)| t.to_string(all_references))
                    .collect::<Vec<_>>()
                    .join(", "),
                func.returns.to_string(all_references)
            ),
            Type::Tuple(inner) => format!("({})", inner.iter()
                .map(|t| t.to_string(all_references))
                .collect::<Vec<_>>()
                .join(", ")),
            Type::Array(inner) => format!("Array<{}>", all_references
                .get(inner)
                .to_string(all_references)),
            Type::Union(types) => types.iter()
                .map(|t| t.to_string(all_references))
                .collect::<Vec<_>>()
                .join(" | "),
            Type::Generic(generic) => {
                generic.to_string(all_references)
            }
            Type::Unknown(unknown) => {
                unknown.to_string(all_references)
            }
            Type::Reference(id) => {
                let t = all_references.get(id);
                t.to_string(all_references)
            }
            Type::_None => "None".to_string()
        }
    }

    /// Get the names of the Generics, that are used by the type
    pub(crate) fn get_used_generics(&self, all_references: &AllReferences) -> Vec<String> {
        match self {
            Type::Lambda(lam) => vec![], // TODO: Implement
            Type::Tuple(inner_types) => inner_types.iter()
                .map(|inner_type| inner_type.get_used_generics(all_references))
                .flatten()
                .collect(),
            Type::Array(inner_ref_id) => all_references.get(inner_ref_id).get_used_generics(all_references),
            Type::Reference(ref_id) => all_references.get(ref_id).get_used_generics(all_references),
            Type::Generic(gen) => vec![gen.name.clone()],
            Type::Union(_) => vec![], // There is no concrete way to know which variant of the union is going to be used. Consider a Type: `Str | T`. T could be used, or not depending on the situation
            _ => vec![]
        }
    }

    pub(crate) fn replace_type_generics(&mut self, generic_map: &HashMap<String, usize>, real_types: &Vec<&Type>, all_references: &mut AllReferences) {
        let generic_name = match self {
            Type::Reference(return_type_id) => match all_references.get(return_type_id) {
                Type::Generic(gen) => Some(gen.name.clone()),
                _ => None
            },
            Type::Generic(gen) => match &gen.base {
                GenericBase::Requirements(_) => unreachable!("Generic requirement have to be behind reference"),
                GenericBase::Specific(_) => Some(gen.name.clone())
            },
            Type::Array(inner_id) => {
                let mut new_inner = all_references.get(inner_id).clone();
                new_inner.replace_type_generics(generic_map, real_types, all_references);
                *inner_id = all_references.insert(new_inner); // Don't always create new array reference (Maybe the replace_return_type_generics didn't do anything)
                None
            },
            Type::Union(inner_types) | Type::Tuple(inner_types) => {
                for inner_type in inner_types {
                    inner_type.replace_type_generics(generic_map, real_types, all_references)
                }
                None
            },
            Type::Lambda(lam) => None, // TODO: Implement
            _ => None
        };
        if let Some(generic_name) = generic_name {
            if let Some(param_index) = generic_map.get(&generic_name) {
                *self = real_types[*param_index].clone();
            }
        }
    }

    fn requirements(&self) -> Option<&TypeRequirements> {
        match self {
            Type::Generic(gen) => match &gen.base {
                GenericBase::Requirements(requirements) => Some(&requirements),
                GenericBase::Specific(_) => None,
            },
            Type::Unknown(req) => Some(req),
            _ => {None}
        }
    }

}