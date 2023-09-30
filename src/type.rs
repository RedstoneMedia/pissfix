use crate::scope::Function;


#[derive(Debug, Clone, Default)]
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
    Generic(String),
    #[default]
    _Unknown, // Only internally used
    _None  // Only internally used
}

impl Type {

    pub(crate) fn union_from(mut a: Type, b: Type) -> Type {
        if a.expect_to_be(&b) {return a} // Can we do this?
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

    pub(crate) fn union_from_all(mut types: Vec<Type>) -> Type {
        if types.len() == 1 {
            return types.pop().unwrap();
        }
        let mut a = types.pop().unwrap();
        while let Some(b) = types.pop() {
            a = Self::union_from(a, b);
        }
        a
    }


    pub(crate) fn try_into_inner_mut(&mut self) -> Option<&mut Self> {
        match self {
            Type::String => Some(self),
            Type::Array(inner) => Some(inner.as_mut()),
            //Type::Union(t) | Type::Tuple(t) => t.iter_mut().collect(),
            Type::_Unknown | Type::Object | Type::Generic(_) => Some(self),
            _ => None
        }
    }

    pub(crate) fn try_into_iter_inner(self) -> Option<Self> {
        match self {
            Type::String => Some(Type::String), // Or Char maybe at some point
            Type::Object | Type::Generic(_) => Some(Type::_Unknown),
            Type::Range => Some(Type::Integer),
            Type::Tuple(types) => Some(Self::union_from_all(types)),
            Type::Array(inner) => Some(*inner),
            Type::Union(types) => {
                let inners : Vec<_> = types.into_iter().filter_map(|t| t.try_into_iter_inner()).collect();
                if inners.is_empty() {
                    None
                } else {
                    Some(Self::union_from_all(inners))
                }
            },
            Type::_Unknown => Some(Type::_Unknown),
            _ => None
        }
    }

    pub(crate) fn expect_to_be(&mut self, expected: &Type) -> bool {
        if let Type::_Unknown | Type::Object = &self {
            *self = expected.clone();
            return true
        }

        match (expected, self) {
            (Type::_Unknown | Type::Object | Type::Generic(_), _) => true,
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
                    if !sub_actual.expect_to_be(sub_expected) {
                        return false;
                    }
                }
                true
            }
            (Type::Tuple(expected_types), actual) => {
                if expected_types.len() == 1 {
                    actual.expect_to_be(&expected_types[0])
                } else {
                    false
                }
            }
            (Type::Array(inner_expected), Type::Array(inner_actual)) => {
                inner_actual.expect_to_be(inner_expected)
            }
            (expected, actual@Type::Union(_)) => {
                let mut types_expected = Vec::new();
                if let Type::Union(subtypes) = expected {
                    types_expected.extend(subtypes.iter());
                } else {
                    types_expected.push(expected)
                }

                let Type::Union(types_actual) = actual else {unreachable!()};

                types_actual.retain_mut(|sub_actual| {
                    types_expected
                        .iter()
                        .any(|sub_expected| sub_actual.expect_to_be(sub_expected))
                });
                let does_overlap = !types_actual.is_empty(); // No overlapping types -> Types don't match
                if types_actual.len() == 1 { // Coerce union of only one type to inner type
                    *actual = types_actual.pop().unwrap();

                }
                does_overlap
            },
            (Type::Union(types_expected), actual) => {
                types_expected
                    .iter()
                    .any(|sub_expected| actual.expect_to_be(sub_expected))
            },
            (_, _) => false
        }
    }

}