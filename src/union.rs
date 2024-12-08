use crate::types::{is_subtype, Type};

pub fn collapse_subtypes(types: Vec<Type>) -> Vec<Type> {
    types
        .iter()
        .enumerate()
        .filter(|&(i1, t1)| {
            types
                .iter()
                .enumerate()
                .all(|(i2, t2)| i1 == i2 || !is_subtype(t1, t2) || (is_subtype(t2, t1) && i1 < i2))
        })
        .map(|(_, t)| t.clone())
        .collect()
}

pub fn flatten(types: Vec<Type>) -> Vec<Type> {
    types
        .into_iter()
        .flat_map(|t| match t {
            Type::Union(ts) => ts,
            _ => vec![t],
        })
        .collect()
}

pub fn union(types: Vec<Type>) -> Type {
    let types = collapse_subtypes(flatten(types));
    if types.is_empty() {
        Type::Never
    } else if types.len() == 1 {
        types[0].clone()
    } else {
        Type::Union(types)
    }
}

#[cfg(test)]
mod tests {
    use crate::types::*;
    use std::vec;

    use super::*;

    #[test]
    fn test_collapse_subtypes() {
        let case = vec![
            (
                vec![Type::Number, Type::String],
                vec![Type::Number, Type::String],
            ),
            (vec![Type::String, Type::String], vec![Type::String]),
            (
                vec![
                    Type::Singleton(SingletonProp {
                        base: Box::new(Type::String),
                        value: Primitive::String("foo".to_string()),
                    }),
                    Type::String,
                ],
                vec![Type::String],
            ),
        ];

        for (input, expected) in case {
            assert_eq!(collapse_subtypes(input), expected);
        }
    }

    #[test]
    fn test_flatten() {
        let case = vec![
            (
                vec![Type::Number, Type::String],
                vec![Type::Number, Type::String],
            ),
            (
                vec![Type::Union(vec![Type::Number, Type::String]), Type::String],
                vec![Type::Number, Type::String, Type::String],
            ),
        ];

        for (input, expected) in case {
            assert_eq!(flatten(input), expected);
        }
    }

    #[test]
    fn test_union() {
        let case = vec![
            (
                vec![Type::Number, Type::String],
                Type::Union(vec![Type::Number, Type::String]),
            ),
            (
                vec![Type::Union(vec![Type::Number, Type::String]), Type::String],
                Type::Union(vec![Type::Number, Type::String]),
            ),
            (
                vec![
                    Type::Singleton(SingletonProp {
                        base: Box::new(Type::String),
                        value: Primitive::String("foo".to_string()),
                    }),
                    Type::String,
                ],
                Type::String,
            ),
            (vec![], Type::Never),
        ];

        for (input, expected) in case {
            assert_eq!(union(input), expected);
        }
    }
}
