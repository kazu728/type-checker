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

pub fn distribute_union(types: Vec<Type>) -> Vec<Vec<Type>> {
    types.into_iter().fold(vec![vec![]], |acc, t| {
        acc.into_iter()
            .flat_map(|current_combination| match &t {
                Type::Union(union_types) => union_types
                    .into_iter()
                    .map(|arm| {
                        let mut expanded_combination = current_combination.clone();
                        expanded_combination.push(arm.clone());
                        expanded_combination
                    })
                    .collect::<Vec<_>>(),
                _ => {
                    let mut expanded_combination = current_combination.clone();
                    expanded_combination.push(t.clone());
                    vec![expanded_combination]
                }
            })
            .collect()
    })
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

    #[test]
    fn test_distribute_union() {
        let case = vec![(
            vec![
                Type::Union(vec![
                    Type::Number,
                    Type::String,
                    Type::Singleton(SingletonProp {
                        base: Box::new(Type::String),
                        value: Primitive::String("foo".to_string()),
                    }),
                ]),
                Type::Union(vec![Type::String, Type::Boolean]),
                Type::String,
            ],
            vec![
                vec![Type::Number, Type::String, Type::String],
                vec![Type::Number, Type::Boolean, Type::String],
                vec![Type::String, Type::String, Type::String],
                vec![Type::String, Type::Boolean, Type::String],
                vec![
                    Type::Singleton(SingletonProp {
                        base: Box::new(Type::String),
                        value: Primitive::String("foo".to_string()),
                    }),
                    Type::String,
                    Type::String,
                ],
                vec![
                    Type::Singleton(SingletonProp {
                        base: Box::new(Type::String),
                        value: Primitive::String("foo".to_string()),
                    }),
                    Type::Boolean,
                    Type::String,
                ],
            ],
        )];

        for (input, expected) in case {
            assert_eq!(distribute_union(input), expected);
        }
    }
}
