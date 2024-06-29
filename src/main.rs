use core::str;
use std::collections::HashMap;

use rslint_parser::ast::Expr;
use rslint_parser::{AstNode, Parse, SyntaxKind};

fn main() {
    println!("Hello, world!");
}

fn parse_expression(input: &str) -> Parse<Expr> {
    rslint_parser::parse_expr(input, 1)
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct Property {
    name: String,
    _type: Type,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum Type {
    Null,
    Boolean,
    Number,
    String,
    Object(Vec<Property>),
    Function { args: Vec<Type>, ret: Box<Type> },
}

impl Type {
    fn is_null(&self) -> bool {
        self == &Type::Null
    }

    fn is_boolean(&self) -> bool {
        self == &Type::Boolean
    }

    fn is_number(&self) -> bool {
        self == &Type::Number
    }

    fn is_string(&self) -> bool {
        self == &Type::String
    }

    fn is_object(&self) -> bool {
        match self {
            Type::Object(_) => true,
            _ => false,
        }
    }
    fn is_function(&self) -> bool {
        match self {
            Type::Function { .. } => true,
            _ => false,
        }
    }

    fn to_string(&self) -> String {
        match self {
            Type::Null => "null".to_string(),
            Type::Boolean => "boolean".to_string(),
            Type::Number => "number".to_string(),
            Type::String => "string".to_string(),
            Type::Object(properties) => {
                let mut s = "{".to_string();
                for p in properties {
                    s.push_str(&format!("{}: {}, ", p.name, p._type.to_string()));
                }
                s.push_str("}");
                s
            }
            Type::Function { args, ret } => {
                let mut s = "(".to_string();
                for a in args {
                    s.push_str(&format!("{}, ", a.to_string()));
                }
                s.push_str(") => ");
                s.push_str(&ret.to_string());
                s
            }
        }
    }

    fn of_ts_type(ast: Parse<Expr>) -> Type {
        match &ast.tree().syntax().kind() {
            SyntaxKind::LITERAL => of_ts_type_literal(&ast.syntax().text().to_string().as_str()),
            SyntaxKind::OBJECT_EXPR => of_ts_type_object(&ast),
            SyntaxKind::ARROW_EXPR => of_ts_type_function(&ast),
            _a => unreachable!("of_ts_type: {:?}", _a),
        }
    }
}
fn synth(env: Env, ast: &Parse<Expr>) -> Type {
    match ast.syntax().kind() {
        SyntaxKind::NAME_REF => synth_identifier(env, ast),
        SyntaxKind::LITERAL => synth_literal(ast),
        SyntaxKind::OBJECT_EXPR => synth_object(env, ast),
        SyntaxKind::ARROW_EXPR => synth_function(env, ast),
        SyntaxKind::CALL_EXPR => synth_call(env, ast),
        SyntaxKind::GROUPING_EXPR => synth_group_expr(env, ast),
        _ => unreachable!("unreachable {:?}", ast.syntax().kind()),
    }
}

fn synth_identifier(env: Env, ast: &Parse<Expr>) -> Type {
    let name = ast.syntax().text().to_string();
    let _type = env.get(&name).unwrap();
    _type.clone()
}

fn synth_literal(ast: &Parse<Expr>) -> Type {
    of_ts_type_literal(ast.syntax().text().to_string().as_str())
}

fn synth_group_expr(env: Env, ast: &Parse<Expr>) -> Type {
    let inner = ast.syntax().children().next().unwrap();
    synth(env, &parse_expression(&inner.text().to_string()))
}

fn synth_object(env: Env, ast: &Parse<Expr>) -> Type {
    let properties = ast
        .syntax()
        .children()
        .map(|node| {
            let key = node.children().next().unwrap().text().to_string();
            let value = node.children().nth(1).unwrap().to_string();
            Property {
                name: key,
                _type: synth(env.clone(), &parse_expression(&value)),
            }
        })
        .collect();
    Type::Object(properties)
}

fn synth_function(env: Env, ast: &Parse<Expr>) -> Type {
    let bindings: Vec<Property> = ast
        .syntax()
        .children()
        .filter(|node| node.kind() == SyntaxKind::PARAMETER_LIST)
        .flat_map(|node| {
            node.children().map(|node| {
                let name = node.text().to_string();
                // rslint_parserが型情報parseしないので一旦固定値で返す
                let _type = Type::Number;
                Property { name, _type }
            })
        })
        .collect();

    let args: Vec<Type> = bindings.iter().map(|p| p._type.clone()).collect();

    let body_env = bindings
        .iter()
        .fold(env, |env, p| env.clone().set(&p.name, p._type.clone()));

    let body = ast.syntax().children().last().unwrap();
    let ret = synth(
        body_env.clone(),
        &parse_expression(&body.text().to_string()),
    );

    Type::Function {
        args,
        ret: Box::new(ret),
    }
}

fn synth_call(_env: Env, _ast: &Parse<Expr>) -> Type {
    // rslint_parserが型情報parseしないので一旦固定値で返す
    Type::Number
}

// Untyped node -> typed node が提供されてない(?)ので、一旦ここで処理する
// Literal parseに実装されている値以外は提供しないので無視する
// @see https://github.com/rslint/rslint/issues/160
fn of_ts_type_literal(text: &str) -> Type {
    match text {
        "null" => Type::Null,
        "true" | "false" => Type::Boolean,
        s if s.parse::<u64>().is_ok() => Type::Number,
        _ => Type::String,
    }
}

fn of_ts_type_object(ast: &Parse<Expr>) -> Type {
    let mut properties = Vec::new();
    for node in ast.syntax().children() {
        let key = node.children().next().unwrap().text().to_string();
        let value = node.children().nth(1).unwrap().text().to_string();

        properties.push(Property {
            name: key,
            _type: of_ts_type_literal(&value),
        });
    }
    return Type::Object(properties);
}

fn of_ts_type_function(ast: &Parse<Expr>) -> Type {
    // rslint parsreが型parseしないので一旦固定値で返す
    Type::Function {
        args: vec![Type::Number, Type::Number],
        ret: Box::new(Type::Number),
    }
}

fn is_subtype(a: Type, b: Type) -> bool {
    match (a, b) {
        (Type::Null, Type::Null) => true,
        (Type::Boolean, Type::Boolean) => true,
        (Type::Number, Type::Number) => true,
        (Type::String, Type::String) => true,
        (Type::Object(a), Type::Object(b)) => {
            for (i, p) in a.iter().enumerate() {
                if p.name != b[i].name || !is_subtype(p._type.clone(), b[i]._type.clone()) {
                    return false;
                }
            }

            return true;
        }
        (
            Type::Function { args, ret },
            Type::Function {
                args: b_args,
                ret: b_ret,
            },
        ) => {
            if args.len() != b_args.len() {
                return false;
            }

            for (i, arg) in args.iter().enumerate() {
                if !is_subtype(arg.clone(), b_args[i].clone()) {
                    return false;
                }
            }

            return is_subtype(*ret.clone(), *b_ret.clone());
        }
        _ => false,
    }
}

enum PropertyType {
    Single(Property),
    Multiple(Vec<Property>),
}
fn object(property: PropertyType) -> Type {
    match property {
        PropertyType::Single(property) => Type::Object(vec![property]),
        PropertyType::Multiple(properties) => Type::Object(properties),
    }
}

fn parse_type(input: &str) -> Type {
    let ast = rslint_parser::parse_expr(input, 1);

    if ast.errors().is_empty() {
        Type::of_ts_type(ast)
    } else {
        panic!("Failed to parse type: {:?}", ast.errors());
    }
}

enum Map {
    New(String, Type),
    Update(HashMap<String, Type>),
}

#[derive(Clone)]
pub struct Env {
    map: HashMap<String, Type>,
}

impl Env {
    pub fn new(map: Map) -> Self {
        match map {
            Map::New(name, _type) => {
                let mut map = HashMap::new();
                map.insert(name, _type);
                Env { map }
            }
            Map::Update(map) => Env { map },
        }
    }
    fn get(&self, name: &str) -> Option<&Type> {
        self.map.get(name)
    }

    fn set(&mut self, name: &str, _type: Type) -> Self {
        self.map.insert(name.to_string(), _type);
        Env::new(Map::Update(self.map.clone()))
    }

    fn entries(&self) -> Vec<(String, Type)> {
        self.map
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_parse_literal_to_type() {
        assert_eq!(of_ts_type_literal("null"), Type::Null);
        assert_eq!(of_ts_type_literal("true"), Type::Boolean);
        assert_eq!(of_ts_type_literal("false"), Type::Boolean);
        assert_eq!(of_ts_type_literal("7"), Type::Number);
        assert_eq!(of_ts_type_literal("hello"), Type::String);
    }

    #[test]
    fn should_parse_object_to_type() {
        let text = of_ts_type_object(&rslint_parser::parse_expr("{n: 9, b: true, s: 'hello'}", 0));
        assert_eq!(
            text,
            Type::Object(vec![
                Property {
                    name: "n".to_string(),
                    _type: Type::Number
                },
                Property {
                    name: "b".to_string(),
                    _type: Type::Boolean
                },
                Property {
                    name: "s".to_string(),
                    _type: Type::String
                }
            ])
        );
    }

    #[test]
    fn should_handle_subtype() {
        assert!(is_subtype(Type::Null, Type::Null));
        assert!(is_subtype(Type::Boolean, Type::Boolean));
        assert!(is_subtype(Type::Number, Type::Number));
        assert!(is_subtype(Type::String, Type::String));
        assert!(is_subtype(
            Type::Object(vec![Property {
                name: "n".to_string(),
                _type: Type::Number
            }]),
            Type::Object(vec![Property {
                name: "n".to_string(),
                _type: Type::Number
            }])
        ));
        assert_eq!(
            is_subtype(
                Type::Object(vec![Property {
                    name: "n".to_string(),
                    _type: Type::Number
                }]),
                Type::Object(vec![Property {
                    name: "n".to_string(),
                    _type: Type::String
                }])
            ),
            false
        );

        assert_eq!(
            is_subtype(
                Type::Object(vec![Property {
                    name: "n".to_string(),
                    _type: Type::Number
                }]),
                Type::Object(vec![
                    Property {
                        name: "n".to_string(),
                        _type: Type::Number
                    },
                    Property {
                        name: "b".to_string(),
                        _type: Type::Boolean
                    }
                ]),
            ),
            true
        );
    }

    #[test]
    fn should_get_env() {
        let env = Env::new(Map::New("x".to_string(), Type::Number));
        assert_eq!(env.get("x"), Some(&Type::Number));
    }

    #[test]
    fn should_set_env() {
        let mut env = Env::new(Map::New("x".to_string(), Type::Number));
        let env = env.set("y", Type::Boolean);
        assert_eq!(env.get("y"), Some(&Type::Boolean));
    }

    #[test]
    fn should_get_entries() {
        let x = "x".to_string();
        let env = &mut Env::new(Map::New(x, Type::Number));
        let env = &mut env.set("y", Type::Boolean);

        let mut entiries = env.entries();
        // entriesは順番が保証されないので検証用を楽にするためソートする
        entiries.sort_by(|a, b| a.0.cmp(&b.0));

        assert_eq!(
            entiries,
            vec![
                ("x".to_string(), Type::Number),
                ("y".to_string(), Type::Boolean)
            ]
        );
    }

    #[test]
    fn should_synth_object() {
        let env = Env::new(Map::New("x".to_string(), Type::Number));
        let ast = rslint_parser::parse_expr("{n: 9, b: x}", 0);
        let _type = synth(env, &ast);
        assert_eq!(
            _type,
            Type::Object(vec![
                Property {
                    name: "n".to_string(),
                    _type: Type::Number
                },
                Property {
                    name: "b".to_string(),
                    _type: Type::Number
                },
            ])
        );
    }

    #[test]
    fn should_synth_function() {
        let env = Env::new(Map::New("x".to_string(), Type::Number));
        let ast = rslint_parser::parse_expr("(x , y) => ({ x: x, y: y })", 0);
        let _type = synth(env, &ast);
        assert_eq!(
            _type,
            Type::Function {
                args: vec![Type::Number, Type::Number],
                ret: Box::new(Type::Object(vec![
                    Property {
                        name: "x".to_string(),
                        _type: Type::Number
                    },
                    Property {
                        name: "y".to_string(),
                        _type: Type::Number
                    },
                ]))
            }
        );
    }

    #[test]
    fn should_synth_call() {
        let env = Env::new(Map::New("x".to_string(), Type::Number));
        let ast = rslint_parser::parse_expr("f(7, 9)", 0);
        let _type = synth(env, &ast);
        assert_eq!(_type, Type::Number);
    }
}
