use swc_ecma_ast::{TsKeywordType, TsKeywordTypeKind, TsType, TsTypeAnn};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Null,
    Boolean,
    Number,
    String,
    Object(Vec<ObjectProp>),
    Function(FunctionProp),
    Singleton(SingletonProp),
    Never,
    Union(Vec<Type>),
    Unknown,
    Intersection(Vec<Type>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Primitive {
    Boolean(bool),
    Number(usize),
    String(String),
}

impl Primitive {
    pub fn to_string(&self) -> String {
        match self {
            Primitive::Boolean(value) => value.to_string(),
            Primitive::Number(value) => value.to_string(),
            Primitive::String(value) => value.clone(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ObjectProp {
    pub name: String,
    pub _type: Type,
}

impl ObjectProp {
    pub fn new(name: &str, _type: Type) -> ObjectProp {
        ObjectProp {
            name: name.to_string(),
            _type,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FunctionProp {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

impl FunctionProp {
    pub fn new(args: Vec<Type>, ret: Type) -> FunctionProp {
        FunctionProp {
            args,
            ret: Box::new(ret),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct SingletonProp {
    pub base: Box<Type>,
    pub value: Primitive,
}

impl SingletonProp {
    pub fn new(base: Type, value: Primitive) -> SingletonProp {
        SingletonProp {
            base: Box::new(base),
            value,
        }
    }
}

impl Type {
    fn is_null(t: &Type) -> bool {
        matches!(t, Type::Null)
    }

    fn is_boolean(t: &Type) -> bool {
        matches!(t, Type::Boolean)
    }

    fn is_number(t: &Type) -> bool {
        matches!(t, Type::Number)
    }

    fn is_string(t: &Type) -> bool {
        matches!(t, Type::String)
    }

    fn is_object(t: &Type) -> bool {
        matches!(t, Type::Object(_))
    }

    fn is_function(t: &Type) -> bool {
        matches!(t, Type::Function { .. })
    }

    pub fn is_singleton(t: &Type) -> bool {
        matches!(t, Type::Singleton { .. })
    }

    fn is_never(t: &Type) -> bool {
        matches!(t, Type::Never)
    }

    fn is_union(t: &Type) -> bool {
        matches!(t, Type::Union(_))
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Null => "null".to_string(),
            Type::Boolean => "boolean".to_string(),
            Type::Number => "number".to_string(),
            Type::String => "string".to_string(),
            Type::Object(props) => Type::to_string_object(props),
            Type::Function(props) => Type::to_string_function(props),
            Type::Singleton(props) => Type::to_string_singleton(props),
            Type::Never => "never".to_string(),
            Type::Union(types) => Type::to_string_union(types),
            Type::Unknown => "unknown".to_string(),
            Type::Intersection(types) => Type::to_string_intersection(types),
        }
    }

    fn to_string_object(props: &Vec<ObjectProp>) -> String {
        let props_str: Vec<String> = props
            .iter()
            .map(|p| format!("{}: {}", p.name, p._type.to_string()))
            .collect();
        format!("{{ {} }}", props_str.join(", "))
    }

    fn to_string_function(props: &FunctionProp) -> String {
        let args_str: Vec<String> = props.args.iter().map(|a| a.to_string()).collect();
        format!("({}) => {}", args_str.join(", "), props.ret.to_string())
    }

    fn to_string_singleton(prop: &SingletonProp) -> String {
        format!("{}: {}", prop.base.to_string(), prop.value.to_string())
    }

    fn to_string_union(types: &Vec<Type>) -> String {
        let types_str: Vec<String> = types.iter().map(|t| t.to_string()).collect();
        types_str.join(" | ")
    }

    fn to_string_intersection(types: &Vec<Type>) -> String {
        let types_str: Vec<String> = types.iter().map(|t| t.to_string()).collect();
        types_str.join(" & ")
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Type::Null => false,
            Type::Boolean => true,
            Type::Number => true,
            Type::String => true,
            Type::Object(_) => true,
            Type::Function(_) => true,
            Type::Singleton(a) => match &a.value {
                Primitive::Boolean(value) => value.clone(),
                Primitive::Number(value) => *value != 0,
                Primitive::String(value) => !value.is_empty(),
            },
            Type::Never => false,
            Type::Union(types) => types.iter().any(|t| t.is_truthy()),
            Type::Unknown => false,
            Type::Intersection(types) => types.iter().all(|t| t.is_truthy()),
        }
    }

    pub fn is_falsy(&self) -> bool {
        !self.is_truthy()
    }

    pub fn map<F: Fn(&Type) -> Type>(t: &Type, fun: &F) -> Type {
        match t {
            Type::Union(types) => {
                let mapped_types = types
                    .into_iter()
                    .map(|u| Type::map(u, fun))
                    .collect::<Vec<Type>>();

                Type::Union(mapped_types)
            }
            _ => fun(t),
        }
    }

    pub fn map2<F: Fn(&Type, &Type) -> Type>(t1: &Type, t2: &Type, fun: &F) -> Type {
        match (t1, t2) {
            (Type::Union(types1), Type::Union(types2)) => {
                let mut mapped_types = vec![];
                for u1 in types1 {
                    for u2 in types2 {
                        mapped_types.push(Type::map2(&u1, &u2, fun));
                    }
                }

                Type::Union(mapped_types)
            }
            (Type::Union(types), t) | (t, Type::Union(types)) => {
                let mapped_types = types
                    .into_iter()
                    .map(|u| Type::map2(&u, &t, fun))
                    .collect::<Vec<Type>>();

                Type::Union(mapped_types)
            }
            _ => fun(t1, t2),
        }
    }
}

pub fn of_ts_type(ts_type: TsTypeAnn) -> Type {
    match *ts_type.type_ann {
        TsType::TsKeywordType(ts_keyword_type) => of_ts_type_keyword(ts_keyword_type),
        _ => unimplemented!("Unsupported type: {:?}", ts_type.type_ann),
    }
}

fn of_ts_type_keyword(ts_keyword: TsKeywordType) -> Type {
    match ts_keyword.kind {
        TsKeywordTypeKind::TsNullKeyword => Type::Null,
        TsKeywordTypeKind::TsBooleanKeyword => Type::Boolean,
        TsKeywordTypeKind::TsNumberKeyword => Type::Number,
        TsKeywordTypeKind::TsStringKeyword => Type::String,
        _ => unimplemented!("Unsupported keyword type: {:?}", ts_keyword.kind),
    }
}

pub fn is_subtype(a: &Type, b: &Type) -> bool {
    match (a, b) {
        (Type::Null, Type::Null) => true,
        (Type::Boolean, Type::Boolean) => true,
        (Type::Number, Type::Number) => true,
        (Type::String, Type::String) => true,
        (a @ Type::Object(_), b @ Type::Object(_)) => is_subtype_object(a, b),
        (a @ Type::Function { .. }, b @ Type::Function { .. }) => is_subtype_function(a, b),
        (Type::Singleton(SingletonProp { base, value }), Type::Number) => {
            Type::is_number(&base) && matches!(value, Primitive::Number(_))
        }
        (Type::Singleton(SingletonProp { base, value }), Type::String) => {
            Type::is_string(&base) && matches!(value, Primitive::String(_))
        }
        (Type::Union(types), b) => types.iter().all(|t| is_subtype(t, b)),
        (_, Type::Union(types)) => types.iter().any(|t| is_subtype(a, t)),

        _ => false,
    }
}

fn is_subtype_object(a: &Type, b: &Type) -> bool {
    if let (Type::Object(a_props), Type::Object(b_props)) = (a, b) {
        for b_prop in b_props {
            let a_prop = a_props.iter().find(|p| p.name == b_prop.name);
            if let Some(a_prop) = a_prop {
                if !is_subtype(&a_prop._type, &b_prop._type) {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    } else {
        false
    }
}

fn is_subtype_function(a: &Type, b: &Type) -> bool {
    if let (
        Type::Function(FunctionProp {
            args: a_args,
            ret: a_ret,
        }),
        Type::Function(FunctionProp {
            args: b_args,
            ret: b_ret,
        }),
    ) = (a, b)
    {
        if a_args.len() != b_args.len() {
            return false;
        }
        for (a_arg, b_arg) in a_args.iter().zip(b_args.iter()) {
            if !is_subtype(a_arg, b_arg) {
                return false;
            }
        }
        is_subtype(a_ret, b_ret)
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn should_handle_subtype() {
        let case = vec![
            (Type::Null, Type::Null, true),
            (Type::Boolean, Type::Boolean, true),
            (Type::Number, Type::Number, true),
            (Type::String, Type::String, true),
            (
                Type::Object(vec![ObjectProp::new("n", Type::Number)]),
                Type::Object(vec![ObjectProp::new("n", Type::Number)]),
                true,
            ),
            (
                Type::Object(vec![ObjectProp::new("n", Type::Number)]),
                Type::Object(vec![ObjectProp::new("n", Type::String)]),
                false,
            ),
            (
                Type::Object(vec![ObjectProp::new("n", Type::Number)]),
                Type::Object(vec![
                    ObjectProp::new("n", Type::Number),
                    ObjectProp::new("b", Type::Boolean),
                ]),
                false,
            ),
            (
                Type::Function(FunctionProp::new(vec![Type::Number], Type::Number)),
                Type::Function(FunctionProp::new(vec![Type::Number], Type::Number)),
                true,
            ),
            (
                Type::Function(FunctionProp::new(vec![Type::Number], Type::Number)),
                Type::Function(FunctionProp::new(vec![Type::String], Type::Number)),
                false,
            ),
            (
                Type::Function(FunctionProp::new(vec![Type::Number], Type::Number)),
                Type::Function(FunctionProp::new(vec![Type::Number], Type::String)),
                false,
            ),
        ];

        for (a, b, expected) in case {
            assert_eq!(is_subtype(&a, &b), expected);
        }
    }
}
