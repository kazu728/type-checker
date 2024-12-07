use swc_ecma_ast::{TsKeywordType, TsKeywordTypeKind, TsType, TsTypeAnn};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Type {
    Null,
    Boolean,
    Number,
    String,
    Object(Vec<ObjectProperty>),
    Function(FunctionProperty),
    Singleton(SingletonProperty),
    Never,
    Union(Vec<Type>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum PrimitiveType {
    Boolean(bool),
    Number(usize),
    String(String),
}

impl PrimitiveType {
    pub fn to_string(&self) -> String {
        match self {
            PrimitiveType::Boolean(value) => value.to_string(),
            PrimitiveType::Number(value) => value.to_string(),
            PrimitiveType::String(value) => value.clone(),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ObjectProperty {
    pub name: String,
    pub _type: Type,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct FunctionProperty {
    pub args: Vec<Type>,
    pub ret: Box<Type>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct SingletonProperty {
    pub base: Box<Type>,
    pub value: PrimitiveType,
}

impl Type {
    fn is_null(&self) -> bool {
        matches!(self, Type::Null)
    }

    fn is_boolean(&self) -> bool {
        matches!(self, Type::Boolean)
    }

    fn is_number(&self) -> bool {
        matches!(self, Type::Number)
    }

    fn is_string(&self) -> bool {
        matches!(self, Type::String)
    }

    fn is_object(&self) -> bool {
        matches!(self, Type::Object(_))
    }

    fn is_function(&self) -> bool {
        matches!(self, Type::Function { .. })
    }

    pub fn is_singleton(&self) -> bool {
        matches!(self, Type::Singleton { .. })
    }

    fn is_never(&self) -> bool {
        matches!(self, Type::Never)
    }

    fn is_union(&self) -> bool {
        matches!(self, Type::Union(_))
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Null => "null".to_string(),
            Type::Boolean => "boolean".to_string(),
            Type::Number => "number".to_string(),
            Type::String => "string".to_string(),
            Type::Object(properties) => {
                let props: Vec<String> = properties
                    .iter()
                    .map(|p| format!("{}: {}", p.name, p._type.to_string()))
                    .collect();
                format!("{{ {} }}", props.join(", "))
            }
            Type::Function(FunctionProperty { args, ret }) => {
                let args_str: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                format!("({}) => {}", args_str.join(", "), ret.to_string())
            }
            Type::Singleton(SingletonProperty { base, value }) => {
                format!("{}: {}", base.to_string(), value.to_string())
            }
            Type::Never => "never".to_string(),
            Type::Union(types) => {
                let types_str: Vec<String> = types.iter().map(|t| t.to_string()).collect();
                types_str.join(" | ")
            }
        }
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
                PrimitiveType::Boolean(value) => value.clone(),
                PrimitiveType::Number(value) => *value != 0,
                PrimitiveType::String(value) => !value.is_empty(),
            },
            Type::Never => false,
            Type::Union(types) => types.iter().any(|t| t.is_truthy()),
        }
    }

    pub fn is_falsy(&self) -> bool {
        !self.is_truthy()
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
        (Type::Singleton(SingletonProperty { base, value }), Type::Number) => {
            base.is_number() && matches!(value, PrimitiveType::Number(_))
        }
        (Type::Singleton(SingletonProperty { base, value }), Type::String) => {
            base.is_string() && matches!(value, PrimitiveType::String(_))
        }

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
        Type::Function(FunctionProperty {
            args: a_args,
            ret: a_ret,
        }),
        Type::Function(FunctionProperty {
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
    use super::*;

    #[test]
    fn should_handle_subtype() {
        assert!(is_subtype(&Type::Null, &Type::Null));
        assert!(is_subtype(&Type::Boolean, &Type::Boolean));
        assert!(is_subtype(&Type::Number, &Type::Number));
        assert!(is_subtype(&Type::String, &Type::String));

        assert!(is_subtype(
            &Type::Object(vec![ObjectProperty {
                name: "n".to_string(),
                _type: Type::Number
            }]),
            &Type::Object(vec![ObjectProperty {
                name: "n".to_string(),
                _type: Type::Number
            }])
        ));

        assert_eq!(
            is_subtype(
                &Type::Object(vec![ObjectProperty {
                    name: "n".to_string(),
                    _type: Type::Number
                }]),
                &Type::Object(vec![ObjectProperty {
                    name: "n".to_string(),
                    _type: Type::String
                }])
            ),
            false
        );

        assert_eq!(
            is_subtype(
                &Type::Object(vec![ObjectProperty {
                    name: "n".to_string(),
                    _type: Type::Number
                }]),
                &Type::Object(vec![
                    ObjectProperty {
                        name: "n".to_string(),
                        _type: Type::Number
                    },
                    ObjectProperty {
                        name: "b".to_string(),
                        _type: Type::Boolean
                    }
                ]),
            ),
            false
        );
    }
}
