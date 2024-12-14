use crate::{
    types::Type,
    union::{distribute_union, union},
};

// ２つの型の交差部分が空でないか(共通して表せる値の集合を持っているか)
pub fn overlaps(x: &Type, y: &Type) -> bool {
    match (x, y) {
        (Type::Never, _) | (_, Type::Never) => false,
        (Type::Unknown, _) | (_, Type::Unknown) => true,

        // union型の場合、Vaiantに構成要素が含まれている場合は重なりがあるか
        (Type::Union(xs), y) => xs.iter().any(|x| overlaps(x, y)),
        (x, Type::Union(ys)) => ys.iter().any(|y| overlaps(x, y)),

        // intersection型の場合、全ての構成要素が重なりがあるかを確認
        (Type::Intersection(xs), y) => xs.iter().all(|x| overlaps(x, y)),
        (x, Type::Intersection(ys)) => ys.iter().all(|y| overlaps(x, y)),

        // singleton型同士の場合、値が一致しているかを確認
        (Type::Singleton(x), Type::Singleton(y)) => x.value == y.value,

        // singleton型とそれ以外の型の場合、それ以外の型がsingleton型の値を含んでいるかを確認
        (Type::Singleton(x), y) => &*x.base == y,
        (x, Type::Singleton(y)) => x == &*y.base,

        // object型同士の場合、それぞれが持つプロパティを比較
        (Type::Object(x_props), Type::Object(y_props)) => x_props.iter().all(|prop| match y_props
            .iter()
            .find(|y_prop| y_prop.name == prop.name)
        {
            Some(y_prop) => overlaps(&prop._type, &y_prop._type),
            None => true,
        }),

        _ => x == y,
    }
}

// 等価な型を1つにまとめる
fn collapse_super_types(types: Vec<Type>) -> Vec<Type> {
    unimplemented!()
}

// ネストされたintersection型をフラットにする
fn flatten(types: Vec<Type>) -> Vec<Type> {
    types
        .into_iter()
        .flat_map(|t| match t {
            Type::Intersection(xs) => xs,
            _ => vec![t],
        })
        .collect()
}

// 数の型からなる配列を交差型にまとめる
fn intersection_no_union(types: Vec<Type>) -> Type {
    unimplemented!()
}

pub fn intersection(types: Vec<Type>) -> Type {
    let types = flatten(types);
    let types = distribute_union(types)
        .into_iter()
        .map(intersection_no_union)
        .collect();

    union(types)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_overlaps() {
        let cases = vec![
            (Type::Never, Type::Never, false),
            (Type::Never, Type::Number, false),
            (
                Type::Union(vec![Type::Number, Type::String]),
                Type::Number,
                true,
            ),
            (
                Type::Union(vec![Type::Number, Type::String]),
                Type::Boolean,
                false,
            ),
            (
                Type::Intersection(vec![Type::Number, Type::String]),
                Type::Number,
                false,
            ),
            (Type::Intersection(vec![Type::Number]), Type::Number, true),
        ];

        for (x, y, expected) in cases {
            assert_eq!(overlaps(&x, &y), expected);
        }
    }
}
