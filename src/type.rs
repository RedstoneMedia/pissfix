use std::collections::HashMap;
use std::usize;
use crate::scope::Function;

/// Holds all types that were Unknown at one point All references
/// A reference type in here can *NEVER* be deleted
#[derive(Debug, Clone, Default)]
pub struct AllReferences {
    pub types: Vec<Type>
}

impl AllReferences {

    pub fn insert(&mut self, t: Type) -> usize {
        self.types.push(t);
        self.types.len() - 1
    }

    pub fn replace(&mut self, id: usize, new: Type) {
        self.types[id] = new;
    }

    pub fn get_mut(&mut self, id: &usize) -> &mut Type {
        self.types.get_mut(*id).expect(&format!("Reference does not exist with id : {}", id))
    }

    pub fn get(&self, id: &usize) -> &Type {
        self.types.get(*id).expect(&format!("Reference does not exist with id : {}", id))
    }

}


#[derive(Debug, Clone, Default)]
pub struct Generic {
    pub name: String,
    pub base: GenericBase
}

#[derive(Debug, Clone)]
pub enum GenericBase {
    Requirements(TypeRequirements),
    Specific(Type)
}

impl Default for GenericBase {
    fn default() -> Self {
        Self::Requirements(Default::default())
    }
}


#[derive(Debug, Clone, Default)]
pub struct TypeRequirements {
    pub requirements: Vec<TypeRequirement>
}

impl TypeRequirements {

    pub fn new(requirements: Option<Vec<TypeRequirement>>, all_unknown: &mut AllReferences) -> Type {
        let id = all_unknown.insert(Type::Unknown(Box::new(Self {
            requirements: requirements.unwrap_or_else(|| Default::default()),
        })));
        Type::Reference(id)
    }

    fn ensure_fulfills(&mut self, t: &Type, all_references: &mut AllReferences, lax_context: bool) -> bool {
        if let Type::Reference(additional_requirements_id) = t {
            if let Some(additional_requirements) = all_references.get(additional_requirements_id).requirements().cloned() {
                for requirement in additional_requirements.requirements {
                    self.require(requirement, all_references, lax_context);
                }
                return true; // Generics are always fulfilled, if the requirements are made to match
            }
        }
        self.requirements.iter()
            .all(|req| {
                req.does_fulfill(t, all_references, lax_context)
            })
    }

    fn require(&mut self, mut requirement: TypeRequirement, all_references: &mut AllReferences, lax_context: bool) {
        let already_required = self.requirements.iter()
            .any(|req| req.eq(&requirement, all_references, lax_context));
        if !already_required {
            // Always create inner generic for Index requirement
            if let TypeRequirement::Index(None) = requirement {
                let inner_id = all_references.insert(Type::Unknown(Default::default()));
                requirement = TypeRequirement::Index(Some(inner_id))
            }
            self.requirements.push(requirement);
        }
    }

    pub fn to_string(&self, all_references: &AllReferences) -> String {
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

    pub fn new_requirement_based(name: String, requirements: Option<TypeRequirements>, all_references: &mut AllReferences) -> Type {
        let id = all_references.insert(Type::Generic(Box::new(Self {
            name,
            base: GenericBase::Requirements(requirements.unwrap_or_else(|| Default::default())),
        })));
        Type::Reference(id)
    }

    pub fn new_specific_based(name: String, specific_type: Type) -> Type {
        Type::Generic(Box::new(Self {
            name,
            base: GenericBase::Specific(specific_type),
        }))
    }

    pub fn to_string(&self, all_references: &AllReferences) -> String {
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

    fn ensure_fulfills(&mut self, t: &Type, all_references: &mut AllReferences, lax_context: bool) -> bool {
        match &mut self.base {
            GenericBase::Requirements(requirements) => requirements.ensure_fulfills(t, all_references, lax_context),
            GenericBase::Specific(specific_type) => t.expect_to_be(specific_type, all_references, lax_context)
        }
    }

    fn require(&mut self, mut requirement: TypeRequirement, all_references: &mut AllReferences, lax_context: bool) {
        match &mut self.base {
            GenericBase::Requirements(requirements) => {
                let already_required = requirements.requirements.iter()
                    .any(|req| req.eq(&requirement, all_references, lax_context));
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
pub enum TypeRequirement {
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

    pub fn does_fulfill(&self, t: &Type, all_references: &mut AllReferences, lax_context: bool) -> bool {
        let r = match (self, t) {
            (_, Type::Reference(id)) => {
                let mut ref_type = Default::default();
                std::mem::swap(&mut ref_type, all_references.get_mut(id));

                let r = match &mut ref_type {
                    Type::Generic(generic) => {
                        generic.require(self.clone(), all_references, lax_context);
                        true
                    },
                    Type::Unknown(requirements) => {
                        requirements.require(self.clone(), all_references, lax_context);
                        true
                    }
                    _ => {
                        self.does_fulfill(&ref_type, all_references, lax_context)
                    }
                };
                std::mem::swap(&mut ref_type, all_references.get_mut(id));
                r
            },
            (_, Type::Union(types)) => {
                types.iter().all(|t| self.does_fulfill(t, all_references, lax_context))
            },
            (Self::Index(None) | Self::Iteration(None), Type::Array(_) | Type::Sequence(_)) => true,
            (Self::Index(Some(inner_ref_id)) | Self::Iteration(Some(inner_ref_id)), Type::Array(inner) | Type::Sequence(inner)) => {
                let mut ref_type = Default::default();
                std::mem::swap(&mut ref_type, all_references.get_mut(inner_ref_id));
                let r = if let Type::Generic(inner_generic) = &mut ref_type {
                    inner_generic.ensure_fulfills(&Type::Reference(*inner), all_references, lax_context)
                } else {
                    ref_type.expect_to_be(&Type::Reference(*inner), all_references, lax_context)
                };
                std::mem::swap(&mut ref_type, all_references.get_mut(inner_ref_id));
                r
            }
            (Self::Index(None) | Self::Iteration(None), Type::String) => true,
            (Self::Index(Some(inner_ref_id)) | Self::Iteration(Some(inner_ref_id)), Type::String) => {
                Type::Reference(*inner_ref_id).expect_to_be(&Type::String, all_references, lax_context) // TODO: Maybe change to Char type (when that exists)
            }
            // Ranges can not be index only iterated over
            (Self::Iteration(None), Type::Range) => true,
            (Self::Iteration(Some(inner_ref_id)), Type::Range) => {
                Type::Reference(*inner_ref_id).expect_to_be(&Type::Integer, all_references, lax_context)
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
                t.expect_to_be(with, all_references, lax_context)
            },
            _ => false
        };
        r
    }

    fn eq(&self, other: &Self, all_references: &mut AllReferences, lax_context: bool) -> bool {
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
        with_a.expect_to_be(with_b, all_references, lax_context)
    }

    pub fn to_string(&self, all_references: &AllReferences) -> String {
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
pub enum Type {
    Integer,
    Float,
    String,
    Boolean,
    Range,
    Lambda(Box<Function>),
    Array(usize),
    Sequence(usize), // Special type that represents any indexable and iterable type with a specific inner type. Aka a String or Array
    Struct(String),
    Enum(String),
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

    pub fn try_into_iter_inner(&self, all_references: &AllReferences) -> Option<Self> {
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
            Type::Array(inner_id) | Type::Sequence(inner_id) => Some(Type::Reference(*inner_id)),
            _ => None
        }
    }

    fn ref_expect_to_be(&self, expected: &Type, all_references: &mut AllReferences, lax_context: bool) -> bool {
        let (reference_id, other_type, swapped) = match (self, expected) {
            (Type::Reference(a_id), Type::Reference(r_id)) => {
                if a_id == r_id {
                    return true;
                }
                (r_id, self, false)
            }
            (_, Type::Reference(id)) => (id, self, false),
            (Type::Reference(id), _) => (id, expected, true),
            _ => panic!("ref_expect_to_be called with no references"),
        };
        let mut ref_type = Default::default();
        std::mem::swap(&mut ref_type, all_references.get_mut(reference_id));

        fn follow_reference_loop<'a>(mut t: &'a Type, all_references: &'a AllReferences, reference_id: &usize) -> Option<&'a Type> {
            while let Type::Reference(id) = t {
                if id == reference_id { return None; }
                t = all_references.get(id);
            }
            Some(t)
        }

        // TODO: Keep "actual, expected" order the same when possible using swapped var (for example possible on specific generics)
        let r = match (other_type, &mut ref_type) {
            (Type::Reference(_), Type::Generic(generic)) => {
                if let Some(actual_ref_type) = follow_reference_loop(other_type, all_references, reference_id) {
                    match actual_ref_type {
                        Type::Generic(_) | Type::Unknown(_) => generic.ensure_fulfills(other_type, all_references, lax_context),
                        _ if lax_context => generic.ensure_fulfills(other_type, all_references, lax_context),
                        _ => false,
                    }
                } else {
                    true // Id's were the same
                }
            },
            (Type::Reference(_), Type::Unknown(requirements)) => {
                if let Some(actual_ref_type) = follow_reference_loop(other_type, all_references, reference_id) {
                    match actual_ref_type {
                        Type::Unknown(_) | Type::Generic(_) => requirements.ensure_fulfills(other_type, all_references, lax_context),
                        _ => {
                            let r = requirements.ensure_fulfills(other_type, all_references, lax_context); // Different from generics: Expect to be is legal not only when the other type is a unknown
                            ref_type = other_type.clone(); // Unknown gets collapsed to real type
                            r
                        }
                    }
                } else {
                    true // Id's were the same
                }
            },
            (_, Type::Generic(generic)) => {
                generic.ensure_fulfills(other_type, all_references, lax_context)
            }
            (_, Type::Unknown(requirements)) => {
                let r = requirements.ensure_fulfills(other_type, all_references, lax_context);
                ref_type = other_type.clone();
                r
            }
            (_, _) => {
                if swapped {
                    ref_type.expect_to_be(&other_type, all_references, lax_context)
                } else {
                    other_type.expect_to_be(&ref_type, all_references, lax_context)
                }
            }
        };
        std::mem::swap(&mut ref_type, all_references.get_mut(reference_id));
        r
    }

    pub fn expect_to_be(&self, expected: &Type, all_references: &mut AllReferences, lax_context: bool) -> bool {
        match (expected, self) {
            (_, Type::Reference(_)) | (Type::Reference(_), _) => {
                self.ref_expect_to_be(expected, all_references, lax_context)
            }
            (Type::Generic(gen), _) => {
                match &gen.base {
                    GenericBase::Requirements(_) => unreachable!("Generic requirements are always behind reference"),
                    GenericBase::Specific(t) => self.expect_to_be(&t, all_references, lax_context)
                }
            }
            (_, Type::Generic(gen)) => {
                match &gen.base {
                    GenericBase::Requirements(_) => unreachable!("Generic requirements are always behind reference"),
                    GenericBase::Specific(t) => t.expect_to_be(expected, all_references, lax_context)
                }
            }
            (Type::Any, _) | (_, Type::Any) if lax_context => true,
            (Type::Any, Type::Any) => true,
            (Type::Integer, Type::Integer) => true,
            (Type::Float, Type::Float) => true,
            (Type::String, Type::String) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Range, Type::Range) => true,
            (Type::_None, Type::_None) => true,
            (Type::Struct(name_a), Type::Struct(name_b)) => name_a == name_b,
            (Type::Enum(name_a), Type::Enum(name_b)) => name_a == name_b,
            (Type::Lambda(expected_fun),  Type::Lambda(actual_fun)) => {
                if expected_fun.parameters.len() != actual_fun.parameters.len() {
                    return false;
                }
                let parameters_match = actual_fun.parameters.iter()
                    .zip(&expected_fun.parameters)
                    .all(|((_, actual_param), (_, expected_param))| actual_param.expect_to_be(expected_param, all_references, lax_context));
                let returns_match = actual_fun.returns.expect_to_be(&expected_fun.returns, all_references, lax_context);
                parameters_match && returns_match
            },
            (Type::Array(inner_expected), Type::Array(inner_actual)) => {
                Type::Reference(*inner_actual).expect_to_be(&Type::Reference(*inner_expected), all_references, lax_context)
            },
            (Type::Sequence(inner_expected), actual) => {
                let Some(actual_inner) = actual.try_into_iter_inner(all_references) else {return false};
                actual_inner.expect_to_be(&Type::Reference(*inner_expected), all_references, lax_context)
            }
            // Unions can only be compared in a lenient context, because the concrete type of the unions at runtime might differ
            (expected, Type::Union(types_actual)) if lax_context => {
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
                        .any(|sub_expected| sub_actual.expect_to_be(sub_expected, all_references, lax_context))
                    )
            },
            (Type::Union(types_expected), actual) if lax_context => {
                types_expected
                    .iter()
                    .any(|sub_expected| actual.expect_to_be(sub_expected, all_references, lax_context))
            },
            (_, _) => false
        }
    }


    pub fn require(&self, requirement: TypeRequirement, all_generics: &mut AllReferences, lax_context: bool) -> Result<(), ()> {
        if !requirement.does_fulfill(self, all_generics, lax_context) {
            return Err(());
        }
        if let Type::Reference(id) = self {
            let mut generic_type = Default::default();
            std::mem::swap(&mut generic_type, all_generics.get_mut(id));
            if let Type::Generic(generic) = &mut generic_type {
                generic.require(requirement, all_generics, lax_context);
            }
            std::mem::swap(&mut generic_type, all_generics.get_mut(id));
        }
        Ok(())
    }

    pub fn to_string(&self, all_references: &AllReferences) -> String {
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
            Type::Array(inner) => format!("Array<{}>", all_references
                .get(inner)
                .to_string(all_references)),
            Type::Sequence(inner) => format!("Sequence<{}>", all_references
                .get(inner)
                .to_string(all_references)),
            Type::Struct(name) | Type::Enum(name) => name.clone(),
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
            Type::_None => "None".to_string(),
        }
    }

    /// Get the names of the Generics, that are used by the type
    pub fn get_used_generics(&self, all_references: &AllReferences) -> Vec<GenericPath> {
        match self {
            Type::Lambda(lam) => lam.parameters.iter()
                .map(|(_,  t)| t.get_used_generics(all_references))
                .enumerate()
                .flat_map(|(i, path)| {
                    path.into_iter().map(move |mut path| {
                        path.inside(GenericPathSegment::Parameter(i));
                        path
                    })
                })
                .chain(lam.returns.get_used_generics(all_references).into_iter()
                    .map(|mut path| {
                        path.inside(GenericPathSegment::LambdaReturn);
                        path
                    })
                )
                .collect(),
            Type::Array(inner_ref_id) | Type::Sequence(inner_ref_id) => all_references.get(inner_ref_id)
                .get_used_generics(all_references)
                .into_iter()
                .map(|mut path| {
                    path.inside(GenericPathSegment::Inner);
                    path
                })
                .collect(),
            Type::Reference(ref_id) => all_references.get(ref_id).get_used_generics(all_references),
            Type::Generic(gen) => vec![GenericPath::new(gen)],
            Type::Union(_) => vec![], // There is no concrete way to know which variant of the union is going to be used. Consider a Type: `Str | T`. T could be used, or not depending on the situation
            _ => vec![]
        }
    }

    pub fn replace_type_generics(&mut self, generic_map: &HashMap<String, GenericPath>, real_types: &Vec<&Type>, all_references: &mut AllReferences) -> Result<(), ()> {
        let generic_name = match self {
            Type::Reference(return_type_id) => match all_references.get(return_type_id) {
                Type::Generic(gen) => Some(gen.name.clone()),
                _ => None
            },
            Type::Generic(gen) => match &gen.base {
                GenericBase::Requirements(_) => unreachable!("Generic requirement have to be behind reference"),
                GenericBase::Specific(_) => Some(gen.name.clone())
            },
            Type::Array(inner_id) | Type::Sequence(inner_id) => {
                let mut new_inner = all_references.get(inner_id).clone();
                new_inner.replace_type_generics(generic_map, real_types, all_references)?;
                *inner_id = all_references.insert(new_inner); // TODO: Don't always create new array reference (Maybe the replace_return_type_generics didn't do anything)
                None
            },
            Type::Union(inner_types) => {
                for inner_type in inner_types {
                    inner_type.replace_type_generics(generic_map, real_types, all_references)?;
                }
                None
            },
            Type::Lambda(lam) => {
                lam.as_mut().returns.replace_type_generics(generic_map, real_types, all_references)?;
                for (_, parameter_type) in &mut lam.parameters {
                    parameter_type.replace_type_generics(generic_map, real_types, all_references)?;
                }
                None
            },
            _ => None
        };
        if let Some(generic_name) = generic_name {
            if let Some(generic_path) = generic_map.get(&generic_name) {
                *self = generic_path.get_real(real_types, all_references).ok_or(())?
            }
        }
        Ok(())
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

#[derive(Debug, Clone)]
pub enum GenericPathSegment {
    Parameter(usize),
    LambdaReturn,
    Inner
}

#[derive(Debug, Clone)]
pub struct GenericPath {
    pub generic_name: String,
    segments_stack: Vec<GenericPathSegment>
}

impl GenericPath {

    pub fn new(generic: &Generic) -> Self {
        Self {
            generic_name: generic.name.clone(),
            segments_stack: vec![],
        }
    }

    pub fn inside(&mut self, segment: GenericPathSegment) {
        self.segments_stack.push(segment);
    }

    fn get_real(&self, real_types: &Vec<&Type>, all_references: &AllReferences) -> Option<Type> {
        let mut last_type : Option<Type> = None;
        for segment in self.segments_stack.iter().rev() {
            let mut new_type = match segment {
                GenericPathSegment::Parameter(param_index) => {
                    if let Some(last_type) = last_type {
                        if let Type::Lambda(function) = last_type {
                            function.parameters[*param_index].1.clone()
                        } else {
                            return None;
                        }
                    } else {
                        real_types[*param_index].clone()
                    }
                },
                GenericPathSegment::LambdaReturn => {
                    if let Type::Lambda(function) = last_type.unwrap() {
                        function.returns.clone()
                    } else {
                        return None;
                    }
                },
                GenericPathSegment::Inner => last_type.unwrap()
                    .try_into_iter_inner(all_references)?
            };
            while let Type::Reference(ref_id) = &new_type {
                new_type = all_references.get(ref_id).clone();
            }
            last_type = Some(new_type);

        }
        last_type
    }

}