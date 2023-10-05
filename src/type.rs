use std::collections::HashMap;
use crate::scope::Function;

/// Holds all types that were Unknown at one point
#[derive(Debug, Clone, Default)]
pub(crate) struct AllUnknown {
    types: HashMap<u64, Type>,
    count: u64
}

impl AllUnknown {

    pub(crate) fn insert(&mut self, generic : Type) -> u64 {
        let new_index = self.count;
        self.types.insert(new_index, generic);
        self.count += 1;
        new_index
    }

    pub(crate) fn get_mut(&mut self, index : &u64) -> &mut Type {
        self.types.get_mut(index).expect(&format!("Unknown type does not exist with id : {}", index))
    }

    pub(crate) fn get(&self, index : &u64) -> &Type {
        self.types.get(index).expect(&format!("Unknown type does not exist with id : {}", index))
    }

}


#[derive(Debug, Clone, Default)]
pub(crate) struct Generic {
    pub name: String,
    pub requirements: Vec<GenericRequirement>
}

impl Generic {

    pub(crate) fn new(name: String, requirements: Option<Vec<GenericRequirement>>, all_generics: &mut AllUnknown) -> Type {
        let id = all_generics.insert(Type::Generic(Box::new(Self {
            name,
            requirements: requirements.unwrap_or_else(|| Default::default()),
        })));
        Type::Reference(id)
    }

    pub(crate) fn to_string(&self, all_generics: &AllUnknown) -> String {
        if self.requirements.is_empty() {
            return if !self.name.is_empty() {
                self.name.clone()
            } else {
                "Any".to_string()
            }
        }
        let requirements_strings : Vec<_> = self.requirements.iter()
            .map(|req| req.to_string(all_generics))
            .collect();
        let requirement_string = requirements_strings.join(" and ");
        let name = if self.name.is_empty() {
            "Type".to_string()
        } else {
            self.name.clone()
        };
        if requirements_strings.len() > 1 {
            format!("{} supporting: ({})", name, requirement_string)
        } else {
            format!("{} supporting {}", name, requirement_string)
        }
    }

    fn ensure_fulfills(&mut self, t: &Type, all_unknown: &mut AllUnknown) -> bool {
        if let Type::Reference(additional_requirements_id) = t {
            if let Type::Generic(gen) = all_unknown.get(additional_requirements_id) {
                let additional_requirements = gen.requirements.clone();
                for requirement in additional_requirements {
                    self.require(requirement, all_unknown);
                }
                return true; // Generics are always fulfilled, if the requirements are made to match
            }
        }
        self.requirements.iter()
            .all(|req| {
                req.does_fulfill(t, all_unknown)
            })
    }

    fn require(&mut self, mut requirement: GenericRequirement, all_unknown: &mut AllUnknown) {
        let already_required = self.requirements.iter()
            .any(|req| req.eq(&requirement, all_unknown));
        if !already_required {
            // Always create inner generic for Index requirement
            if let GenericRequirement::Index(None) = requirement {
                let inner_id = all_unknown.insert(Type::Generic(Box::new(Generic { name: "".to_string(), requirements: vec![] })));
                requirement = GenericRequirement::Index(Some(inner_id))
            }
            self.requirements.push(requirement);
        }
    }

}

#[derive(Debug, Clone)]
pub(crate) enum GenericRequirement {
    Index(Option<u64>),
    Equality(Type),
    Addition(Type),
    Subtraction(Type),
    Multiplication(Type),
    Division(Type),
    BooleanNegation,
    Negation
}

impl GenericRequirement {

    pub fn does_fulfill(&self, t: &Type, all_unknown: &mut AllUnknown) -> bool {
        let b = match (self, t) {
            (_, Type::Reference(id)) => {
                let mut ref_type = Default::default();
                std::mem::swap(&mut ref_type, all_unknown.get_mut(id));
                let r = if let Type::Generic(generic) = &mut ref_type {
                    generic.ensure_fulfills(t, all_unknown)
                } else {
                    self.does_fulfill(&ref_type, all_unknown)
                };
                std::mem::swap(&mut ref_type, all_unknown.get_mut(id));
                r
            },
            (_, Type::Union(types)) => {
                types.iter().all(|t| self.does_fulfill(t, all_unknown))
            },
            (Self::Index(None), Type::Array(_)) => true,
            (Self::Index(Some(inner_ref_id)), Type::Array(inner)) => {
                let mut ref_type = Default::default();
                std::mem::swap(&mut ref_type, all_unknown.get_mut(inner_ref_id));
                let r = if let Type::Generic(inner_generic) = &mut ref_type {
                    inner_generic.ensure_fulfills(inner, all_unknown)
                } else {
                    ref_type.expect_to_be(inner, all_unknown)
                };
                std::mem::swap(&mut ref_type, all_unknown.get_mut(inner_ref_id));
                r
            }
            (Self::Index(None), Type::String) => true,
            (Self::Index(Some(inner_ref_id)), Type::String) => {
                Type::Reference(*inner_ref_id).expect_to_be(&Type::String, all_unknown) // TODO: Maybe change to Char type (when that exists)
            }
            (Self::BooleanNegation, Type::Boolean) => true,
            (Self::Negation, Type::Integer | Type::Float) => true,
            (
                Self::Equality(with)
                | Self::Addition(with)
                | Self::Subtraction(with)
                | Self::Multiplication(with)
                | Self::Division(with)
                , _
            ) => {
                t.expect_to_be(with, all_unknown)
            },
            _ => false
        };
        b
    }

    fn eq(&self, other: &Self, all_unknown: &mut AllUnknown) -> bool {
        if let (GenericRequirement::Index(id_a), GenericRequirement::Index(id_b)) = (self, other) {
            if id_a.is_none() || id_b.is_none() {
                return true;
            }
            return id_a == id_b;
        }
        let (with_a, with_b) = match (self, other) {
            (GenericRequirement::Equality(a), GenericRequirement::Equality(b)) => (a, b),
            (GenericRequirement::Addition(a), GenericRequirement::Addition(b)) => (a, b),
            (GenericRequirement::Subtraction(a), GenericRequirement::Subtraction(b)) => (a, b),
            (GenericRequirement::Multiplication(a), GenericRequirement::Multiplication(b)) => (a, b),
            (GenericRequirement::Division(a), GenericRequirement::Division(b)) => (a, b),
            (_, _) => return false
        };
        with_a.expect_to_be(with_b, all_unknown)
    }

    pub(crate) fn to_string(&self, all_unknown: &AllUnknown) -> String {
        match self {
            GenericRequirement::Index(id) => {
                if let Some(id) = id {
                    let generic = all_unknown.get(id);
                    format!("Indexing with inner: [{}]", generic.to_string(all_unknown))
                } else {
                    "Indexing".to_string()
                }
            },
            GenericRequirement::Equality(with) => format!("Equality with {:?}", with),
            GenericRequirement::Addition(with) => format!("Addition with {:?}", with),
            GenericRequirement::Subtraction(with) => format!("Subtraction with {:?}", with),
            GenericRequirement::Multiplication(with) => format!("Multiplication with {:?}", with),
            GenericRequirement::Division(with) => format!("Division with {:?}", with),
            GenericRequirement::Negation => "Negation".to_string(),
            GenericRequirement::BooleanNegation => "Logical NOT".to_string(),
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
    Array(Box<Type>),
    Union(Vec<Type>),
    Reference(u64),
    // This type should *always* be behind a Reference Type
    Generic(Box<Generic>),
    #[default]
    _None  // Only internally used
}

impl Type {

    pub(crate) fn try_into_inner_mut(&mut self) -> Option<&mut Self> {
        match self {
            Type::String => Some(self),
            Type::Array(inner) => Some(inner.as_mut()),
            Type::Generic {..} => Some(self),
            _ => None
        }
    }

    pub(crate) fn try_into_iter_inner(&self, all_unknown: &AllUnknown) -> Option<Self> {
        match self {
            Type::String => Some(Type::String), // Or Char maybe at some point
            Type::Reference(id) => {
                let ref_type = all_unknown.get(id);
                ref_type.try_into_iter_inner(all_unknown)
            }
            Type::Generic(generic) => {
                generic.requirements.iter()
                    .find_map(|req| if let GenericRequirement::Index(inner_ref_type) = req {*inner_ref_type} else {None})
                    .map(|id| Type::Reference(id))
            },
            Type::Range => Some(Type::Integer),
            Type::Tuple(types) => unimplemented!(),
            Type::Array(inner) => Some(*inner.clone()),
            Type::Union(types) => unimplemented!(),
            _ => None
        }
    }

    pub(crate) fn expect_to_be(&self, expected: &Type, all_unknown: &mut AllUnknown) -> bool {
        if let Type::Reference(id) = expected {
            let mut ref_type = Default::default();
            std::mem::swap(&mut ref_type, all_unknown.get_mut(id));
            let r = if let Type::Generic(generic) = &mut ref_type {
                generic.ensure_fulfills(self, all_unknown)
            } else {
                ref_type.expect_to_be(self, all_unknown)
            };
            std::mem::swap(&mut ref_type, all_unknown.get_mut(id));
            return r;
        }
        if let Type::Reference(id) = self {
            let mut ref_type = Default::default();
            std::mem::swap(&mut ref_type, all_unknown.get_mut(id));
            let r = if let Type::Generic(generic) = &mut ref_type {
                generic.ensure_fulfills(expected, all_unknown)
            } else {
                ref_type.expect_to_be(expected, all_unknown)
            };
            std::mem::swap(&mut ref_type, all_unknown.get_mut(id));
            return r;
        }

        match (expected, self) {
            (Type::Generic {..}, _) => unreachable!("Generics are always behind a Reference Type"),
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
                    if !sub_actual.expect_to_be(sub_expected, all_unknown) {
                        return false;
                    }
                }
                true
            }
            (Type::Tuple(expected_types), actual) => {
                if expected_types.len() == 1 {
                    actual.expect_to_be(&expected_types[0], all_unknown)
                } else {
                    false
                }
            }
            (Type::Array(inner_expected), Type::Array(inner_actual)) => {
                inner_actual.expect_to_be(inner_expected, all_unknown)
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
                        .any(|sub_expected| sub_actual.expect_to_be(sub_expected, all_unknown))
                    )
            },
            (Type::Union(types_expected), actual) => {
                types_expected
                    .iter()
                    .any(|sub_expected| actual.expect_to_be(sub_expected, all_unknown))
            },
            (_, _) => false
        }
    }


    pub(crate) fn require(&self, requirement: GenericRequirement, all_generics: &mut AllUnknown) -> Result<(), ()> {
        if !requirement.does_fulfill(&self, all_generics) {
            return Err(());
        }
        if let Type::Reference(id) = self {
            let mut generic_type = Default::default();
            std::mem::swap(&mut generic_type, all_generics.get_mut(id));
            if let Type::Generic(generic) = &mut generic_type {
                generic.require(requirement, all_generics);
            }
            std::mem::swap(&mut generic_type, all_generics.get_mut(id));
        }
        Ok(())
    }

    pub(crate) fn to_string(&self, all_unknown: &AllUnknown) -> String {
        match self {
            Type::Integer => "Integer".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Boolean => "Boolean".to_string(),
            Type::Range => "Range".to_string(),
            Type::Lambda(func) => format!(
                "Lambda ({}) -> {}",
                func.parameters.iter()
                    .map(|(_, t)| t.to_string(all_unknown))
                    .collect::<Vec<_>>()
                    .join(", "),
                func.returns.to_string(all_unknown)
            ),
            Type::Tuple(inner) => format!("({:?})", inner.iter()
                .map(|t| t.to_string(all_unknown))
                .collect::<Vec<_>>()
                .join(", ")),
            Type::Array(inner) => format!("Array<{}>", inner.to_string(all_unknown)),
            Type::Union(types) => types.iter()
                .map(|t| t.to_string(all_unknown))
                .collect::<Vec<_>>()
                .join(" | "),
            Type::Generic(generic) => {
                generic.to_string(all_unknown)
            }
            Type::Reference(id) => {
                let t = all_unknown.get(id);
                t.to_string(all_unknown)
            }
            Type::_None => "None".to_string()
        }
    }

}