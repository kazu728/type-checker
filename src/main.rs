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
}

// TOOD: type predicates
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
        }
    }

    fn of_ts_type(ast: Parse<Expr>) -> Type {
        match &ast.tree().syntax().kind() {
            SyntaxKind::LITERAL => of_ts_type_literal(&ast.syntax().text().to_string().as_str()),
            SyntaxKind::OBJECT_EXPR => of_ts_type_object(&ast),
            _ => unreachable!(),
        }
    }
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

fn is_subtype(a: Type, b: Type) -> bool {
    match (a, b) {
        (Type::Null, Type::Null) => true,
        (Type::Boolean, Type::Boolean) => true,
        (Type::Number, Type::Number) => true,
        (Type::String, Type::String) => true,
        (Type::Object(a), Type::Object(b)) => {
            for (i, p) in a.iter().enumerate() {
                // TOOD: clone s
                if p.name != b[i].name || !is_subtype(p._type.clone(), b[i]._type.clone()) {
                    return false;
                }
            }

            return true;
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
        // TODO: report bug
        panic!("Failed to parse type")
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
}
