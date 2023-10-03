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


#[derive(Debug, Clone, Default)]
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

    pub(crate) fn to_string(&self, all_generics: &AllGenerics) -> String {
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

    fn ensure_fulfills(&mut self, t: &Type, all_generics: &mut AllGenerics) -> bool {
        if let Type::Generic(additional_requirements_id) = t {
            let additional_requirements = all_generics.get(additional_requirements_id).requirements.clone();
            for requirement in additional_requirements {
                self.require(requirement, all_generics);
            }
            return true; // Generics are always fulfilled, if the requirements are made to match
        }
        self.requirements.iter()
            .all(|req| {
                req.does_fulfill(t, all_generics)
            })
    }

    fn require(&mut self, requirement: GenericRequirement, all_generics: &mut AllGenerics) {
        let already_required = self.requirements.iter()
            .any(|req| req.eq(&requirement, all_generics));
        if !already_required {
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

    pub fn does_fulfill(&self, t: &Type, all_generics: &mut AllGenerics) -> bool {
        let b = match (self, t) {
            (_, Type::Generic(_)) => true,
            (_, Type::Union(types)) => {
                types.iter().all(|t| self.does_fulfill(t, all_generics))
            },
            (Self::Index(None), Type::Array(_)) => true,
            (Self::Index(Some(inner_generic_id)), Type::Array(inner)) => {
                let mut generic = Default::default();
                std::mem::swap(&mut generic, all_generics.get_mut(inner_generic_id));
                let r = generic.ensure_fulfills(inner, all_generics);
                std::mem::swap(&mut generic, all_generics.get_mut(inner_generic_id));
                r
            }
            (Self::Index(None), Type::String) => true,
            (Self::Index(Some(inner_generic_id)), Type::String) => {
                let mut generic = Default::default();
                std::mem::swap(&mut generic, all_generics.get_mut(inner_generic_id));
                let r = generic.ensure_fulfills(&Type::String, all_generics); // TODO: Maybe change to Char type (when that exists)
                std::mem::swap(&mut generic, all_generics.get_mut(inner_generic_id));
                r
            },
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
                t.expect_to_be(with, all_generics)
            },
            _ => false
        };
        b
    }

    fn eq(&self, other: &Self, all_generics: &mut AllGenerics) -> bool {
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
        with_a.expect_to_be(with_b, all_generics)
    }

    pub(crate) fn to_string(&self, all_generics: &AllGenerics) -> String {
        match self {
            GenericRequirement::Index(id) => {
                if let Some(id) = id {
                    let generic = all_generics.get(id);
                    format!("Indexing with inner: [{}]", generic.to_string(all_generics))
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

#[derive(Debug, Clone)]
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
    Generic(u64),
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
            Type::Range => Some(Type::Integer),
            Type::Tuple(types) => unimplemented!(),
            Type::Array(inner) => Some(*inner),
            Type::Union(types) => unimplemented!(),
            _ => None
        }
    }

    pub(crate) fn expect_to_be(&self, expected: &Type, all_generics: &mut AllGenerics) -> bool {
        if let Type::Generic(id) = expected {
            let mut generic = Default::default();
            std::mem::swap(&mut generic, all_generics.get_mut(id));
            let r = generic.ensure_fulfills(self, all_generics);
            std::mem::swap(&mut generic, all_generics.get_mut(id));
            return r;
        }
        if let Type::Generic(id) = self {
            let mut generic = Default::default();
            std::mem::swap(&mut generic, all_generics.get_mut(id));
            let r = generic.ensure_fulfills(expected, all_generics);
            std::mem::swap(&mut generic, all_generics.get_mut(id));
            return r;
        }

        match (expected, self) {
            (Type::Generic {..}, _) => true,
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
            let mut generic = Default::default();
            std::mem::swap(&mut generic, all_generics.get_mut(id));
            generic.require(requirement, all_generics);
            std::mem::swap(&mut generic, all_generics.get_mut(id));
        }
        Ok(())
    }

    pub(crate) fn to_string(&self, all_generics: &AllGenerics) -> String {
        match self {
            Type::Integer => "Integer".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Boolean => "Boolean".to_string(),
            Type::Range => "Range".to_string(),
            Type::Lambda(func) => format!(
                "Lambda ({}) -> {}",
                func.parameters.iter()
                    .map(|(_, t)| t.to_string(all_generics))
                    .collect::<Vec<_>>()
                    .join(", "),
                func.returns.to_string(all_generics)
            ),
            Type::Tuple(inner) => format!("({:?})", inner.iter()
                .map(|t| t.to_string(all_generics))
                .collect::<Vec<_>>()
                .join(", ")),
            Type::Array(inner) => format!("Array<{}>", inner.to_string(all_generics)),
            Type::Union(types) => types.iter()
                .map(|t| t.to_string(all_generics))
                .collect::<Vec<_>>()
                .join(" | "),
            Type::Generic(generic_id) => {
                let generic = all_generics.get(generic_id);
                generic.to_string(all_generics)
            }
            Type::_None => "None".to_string()
        }
    }

}