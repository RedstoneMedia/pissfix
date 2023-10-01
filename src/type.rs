use std::collections::HashMap;
use crate::scope::Function;

#[derive(Debug, Clone, Default)]
pub(crate) struct AllGenerics {
    generics: HashMap<u64, Generic>,
    count: u64
}

impl AllGenerics {

    pub(crate) fn insert(&mut self, generic : Generic) -> u64 {
        let new_index = self.count;
        self.generics.insert(new_index, generic);
        self.count += 1;
        new_index
    }

    pub(crate) fn get_mut(&mut self, index : &u64) -> &mut Generic {
        self.generics.get_mut(index).expect(&format!("Generic does not exist with id : {}", index))
    }

    pub(crate) fn get(&self, index : &u64) -> &Generic {
        self.generics.get(index).expect(&format!("Generic does not exist with id : {}", index))
    }

}

#[derive(Debug, Clone)]
pub(crate) struct Generic {
    pub name: String,
    pub requirements: Vec<GenericRequirement>
}

impl Generic {

    pub(crate) fn new(name: String, requirements: Option<Vec<GenericRequirement>>, all_generics: &mut AllGenerics) -> Type {
        let id = all_generics.insert(Self {
            name,
            requirements: requirements.unwrap_or_else(|| Default::default()),
        });
        Type::Generic(id)
    }

}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum GenericRequirement {
    Index(Option<u64>),
    Equality,
    Addition,
    Subtraction,
    Multiplication,
    Division
}

impl GenericRequirement {

    pub fn does_fulfill(&self, t: &Type, all_generics: &AllGenerics) -> bool {
        match (self, t) {
            (_, Type::Object | Type::Generic {..}) => true,
            (_, Type::Union(types)) => {
                types.iter().all(|t| self.does_fulfill(t, all_generics))
            },
            (Self::Index(None), Type::Array(_)) => true,
            (Self::Index(Some(inner_generic_id)), Type::Array(inner)) => {
                let inner_reqs = &all_generics.get(inner_generic_id).requirements;
                inner_reqs.iter()
                    .all(|req| req.does_fulfill(inner, all_generics))
            }
            (Self::Index(None), Type::String) => true,
            (Self::Index(Some(inner_generic_id)), Type::String) => {
                let inner_reqs = &all_generics.get(inner_generic_id).requirements;
                !inner_reqs.iter()
                    .any(|req| matches!(req, GenericRequirement::Subtraction | GenericRequirement::Division | GenericRequirement::Multiplication | GenericRequirement::Index(_)))
            },
            (Self::Equality, Type::String | Type::Integer | Type::Boolean) => true,
            (Self::Addition, Type::Integer | Type::Float | Type::String) => true,
            (Self::Subtraction | Self::Multiplication | Self::Division, Type::Integer | Type::Float) => true,
            _ => false
        }
    }

}

#[derive(Debug, Clone)]
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
    Generic(u64),
    _None  // Only internally used
}

impl Type {

    pub(crate) fn try_into_inner_mut(&mut self) -> Option<&mut Self> {
        match self {
            Type::String => Some(self),
            Type::Array(inner) => Some(inner.as_mut()),
            //Type::Union(t) | Type::Tuple(t) => t.iter_mut().collect(),
            Type::Object | Type::Generic {..} => Some(self),
            _ => None
        }
    }

    pub(crate) fn try_into_iter_inner(self, all_generics: &mut AllGenerics) -> Option<Self> {
        match self {
            Type::String => Some(Type::String), // Or Char maybe at some point
            Type::Generic(id) => {
                let requirements = &mut all_generics.get_mut(&id).requirements;
                let requirement_index = requirements.iter().enumerate().find_map(|(i, req)| {
                    if let GenericRequirement::Index(_) = req {
                        Some(i)
                    } else {None}
                }).unwrap_or_else(|| {
                    requirements.push(GenericRequirement::Index(None));
                    requirements.len() - 1
                });
                let GenericRequirement::Index(generic_inner_id) = &requirements[requirement_index] else {unreachable!()};
                Some(if let Some(generic_inner_id) = generic_inner_id {
                    Type::Generic(*generic_inner_id)
                } else {
                    let inner_generic = Generic::new("".to_string(), None, all_generics);
                    let Type::Generic(inner_id) = &inner_generic else { unreachable!() };
                    let GenericRequirement::Index(generic_inner_id) = &mut all_generics.get_mut(&id).requirements[requirement_index] else {unreachable!()};
                    *generic_inner_id = Some(*inner_id);
                    inner_generic
                })
            },
            Type::Object => Some(Type::Object),
            Type::Range => Some(Type::Integer),
            Type::Tuple(types) => unimplemented!(),
            Type::Array(inner) => Some(*inner),
            Type::Union(types) => unimplemented!(),
            _ => None
        }
    }

    pub(crate) fn expect_to_be(&self, expected: &Type, all_generics: &mut AllGenerics) -> bool {
        if let Type::Generic(id) = self {
            return all_generics.get(id).requirements.iter()
                .all(|req| req.does_fulfill(expected, all_generics))
        }
        if let Type::Generic(id) = expected {
            return all_generics.get(id).requirements.iter()
                .all(|req| req.does_fulfill(self, all_generics))
        }

        match (expected, self) {
            (Type::Object | Type::Generic {..}, _) => true,
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
                    if !sub_actual.expect_to_be(sub_expected, all_generics) {
                        return false;
                    }
                }
                true
            }
            (Type::Tuple(expected_types), actual) => {
                if expected_types.len() == 1 {
                    actual.expect_to_be(&expected_types[0], all_generics)
                } else {
                    false
                }
            }
            (Type::Array(inner_expected), Type::Array(inner_actual)) => {
                inner_actual.expect_to_be(inner_expected, all_generics)
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
                        .any(|sub_expected| sub_actual.expect_to_be(sub_expected, all_generics))
                    )
            },
            (Type::Union(types_expected), actual) => {
                types_expected
                    .iter()
                    .any(|sub_expected| actual.expect_to_be(sub_expected, all_generics))
            },
            (_, _) => false
        }
    }


    pub(crate) fn require(&self, requirement: GenericRequirement, all_generics: &mut AllGenerics) -> Result<(), ()> {
        if !requirement.does_fulfill(&self, all_generics) {
            return Err(());
        }
        if let Type::Generic(id) = self {
            let requirements = &mut all_generics.get_mut(id).requirements;
            if !requirements.contains(&requirement) {
                requirements.push(requirement);
            }
        }
        Ok(())
    }

}