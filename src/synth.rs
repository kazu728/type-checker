use crate::env::Env;
use crate::types::{
    is_subtype, of_ts_type, FunctionProp, ObjectProp, Primitive, SingletonProp, Type,
};
use swc_ecma_ast::{
    ArrowExpr, BinaryOp, BlockStmtOrExpr, CallExpr, Callee, Expr, Ident, Lit, ObjectLit, Pat, Prop,
    PropName, PropOrSpread, Stmt, UnaryOp,
};

pub fn synth(env: &Env, expr: &Expr) -> Type {
    match expr {
        Expr::Ident(ident) => synth_identifier(env, ident),
        Expr::Lit(lit) => synth_literal(lit),
        Expr::Object(obj) => synth_object(env, obj),
        Expr::Member(expr) => synth_member(env, &expr),
        Expr::Arrow(arrow) => synth_function(env, arrow),
        Expr::Call(call) => synth_call(&env, call),
        Expr::Paren(paren) => synth(env, &paren.expr),
        Expr::Bin(bin) => synth_bin(env, bin),
        Expr::Unary(expr) => synth(env, &expr.arg),
        _ => unimplemented!("Unsupported expression: {:?}", expr),
    }
}

fn synth_identifier(env: &Env, ident: &Ident) -> Type {
    let name = ident.sym.to_string();
    let _type = env.get(&name).unwrap();
    _type.clone()
}

fn synth_literal(lit: &Lit) -> Type {
    match lit {
        Lit::Null(_) => Type::Null,
        Lit::Bool(value) => synth_boolean(value),
        Lit::Num(value) => synth_number(value),
        Lit::Str(value) => synth_string(value),
        _ => unimplemented!("Unsupported literal: {:?}", lit),
    }
}

fn synth_boolean(ast: &swc_ecma_ast::Bool) -> Type {
    Type::Singleton(SingletonProp {
        base: Box::new(Type::Boolean),
        value: Primitive::Boolean(ast.value),
    })
}

fn synth_number(ast: &swc_ecma_ast::Number) -> Type {
    Type::Singleton(SingletonProp {
        base: Box::new(Type::Number),
        value: Primitive::Number(ast.value as usize),
    })
}

fn synth_string(ast: &swc_ecma_ast::Str) -> Type {
    Type::Singleton(SingletonProp {
        base: Box::new(Type::String),
        value: Primitive::String(ast.value.to_string()),
    })
}

fn synth_object(env: &Env, obj: &ObjectLit) -> Type {
    let properties = obj
        .props
        .iter()
        .filter_map(|prop| {
            if let PropOrSpread::Prop(boxed_prop) = prop {
                if let Prop::KeyValue(kv) = &**boxed_prop {
                    let key = match &kv.key {
                        PropName::Ident(ident) => ident.sym.to_string(),
                        PropName::Str(str_) => str_.value.to_string(),
                        _ => unimplemented!("Unexpected key type: {:?}", kv.key),
                    };
                    let value_type = synth(env, &kv.value);
                    Some(ObjectProp {
                        name: key,
                        _type: value_type,
                    })
                } else {
                    unimplemented!("Unexpected prop type: {:?}", prop);
                }
            } else {
                None
            }
        })
        .collect();
    Type::Object(properties)
}

fn synth_member(env: &Env, ast: &swc_ecma_ast::MemberExpr) -> Type {
    match &ast.prop {
        swc_ecma_ast::MemberProp::Ident(ident) => {
            let object = synth(env, &ast.obj);

            Type::map(object, &|object| {
                if let Type::Object(properties) = object {
                    properties
                        .into_iter()
                        .find(|p| p.name == ident.sym.to_string())
                        .map(|p| p._type)
                        .unwrap()
                } else {
                    unimplemented!("Expected object type but got {:?}", object)
                }
            })
        }
        _ => unimplemented!("Unexpected member type: {:?}", &ast.prop),
    }
}

fn synth_function(env: &Env, arrow: &ArrowExpr) -> Type {
    let mut bindings: Vec<Type> = Vec::new();
    let mut new_env = env.clone();

    for param in &arrow.params {
        if let Pat::Ident(ident) = param {
            let _type = match param.clone().ident().unwrap().type_ann {
                Some(ts_type) => of_ts_type(*ts_type),
                None => Type::Null,
            };

            bindings.push(_type.clone());
            new_env.set(&ident.id.sym.to_string(), _type);
        } else {
            unimplemented!("Unexpected params type: {:?}", param);
        }
    }

    let ret = match &*arrow.body {
        BlockStmtOrExpr::Expr(expr) => synth(&new_env, expr),
        BlockStmtOrExpr::BlockStmt(block) => {
            if let Some(stmt) = block.stmts.last() {
                if let Stmt::Expr(expr_stmt) = stmt {
                    synth(&new_env, &expr_stmt.expr)
                } else {
                    unimplemented!("Unexpected statement type: {:?}", stmt);
                }
            } else {
                Type::Null
            }
        }
    };

    Type::Function(FunctionProp::new(bindings, ret))
}

fn synth_call(env: &Env, call: &CallExpr) -> Type {
    // 呼び出される側の関数の型を取得
    let callee = match &call.callee {
        Callee::Expr(expr) => synth(env, &*expr),
        _ => panic!("Unexpected callee type: {:?}", call.callee),
    };

    let arg_types: Vec<Type> = call.args.iter().map(|arg| synth(env, &arg.expr)).collect();

    match callee {
        Type::Function(FunctionProp { args, ret }) => {
            if arg_types.len() != args.len() {
                panic!(
                    "Number of arguments does not match: expected {} but got {}",
                    args.len(),
                    arg_types.len()
                );
            }

            for (i, (expected, actual)) in args.iter().zip(arg_types.iter()).enumerate() {
                if !is_subtype(actual, expected) {
                    panic!(
                        "Type mismatch at argument {}: expected {} but got {}",
                        i + 1,
                        expected.to_string(),
                        actual.to_string()
                    )
                }
            }

            *ret
        }
        _ => panic!("Expected function type but got {:?}", callee),
    }
}

fn synth_bin(env: &Env, bin: &swc_ecma_ast::BinExpr) -> Type {
    let left = synth(env, &bin.left);
    let right = synth(env, &bin.right);

    match bin.op {
        BinaryOp::EqEqEq => match (left, right) {
            (Type::Singleton(left), Type::Singleton(right)) => Type::Singleton(SingletonProp {
                base: Box::new(Type::Boolean),
                value: Primitive::Boolean(left.value == right.value),
            }),
            _ => Type::Boolean,
        },
        BinaryOp::NotEqEq => match (left, right) {
            (Type::Singleton(left), Type::Singleton(right)) => Type::Singleton(SingletonProp {
                base: Box::new(Type::Boolean),
                value: Primitive::Boolean(left.value != right.value),
            }),
            _ => Type::Boolean,
        },
        BinaryOp::Add => {
            if is_subtype(&left, &Type::Number) && is_subtype(&right, &Type::Number) {
                match (&left, &right) {
                    (Type::Singleton(left), Type::Singleton(right)) => {
                        Type::Singleton(SingletonProp {
                            base: Box::new(Type::Number),
                            value: Primitive::Number(match (&left.value, &right.value) {
                                (Primitive::Number(left), Primitive::Number(right)) => left + right,
                                _ => unreachable!(),
                            }),
                        })
                    }
                    _ => Type::Number,
                }
            } else {
                unimplemented!("Unsupported binary operator: {:?}", bin.op);
            }
        }
        BinaryOp::LogicalAnd => {
            if left.is_falsy() {
                left
            } else {
                right
            }
        }
        BinaryOp::LogicalOr => {
            if left.is_truthy() {
                left
            } else {
                right
            }
        }

        _ => unimplemented!("Unsupported binary operator: {:?}", bin.op),
    }
}

fn synth_unary(env: &Env, unary: &swc_ecma_ast::UnaryExpr) -> Type {
    let arg = synth(env, &unary.arg);

    match unary.op {
        UnaryOp::Bang => {
            if arg.is_truthy() {
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::Boolean),
                    value: Primitive::Boolean(false),
                })
            } else if arg.is_falsy() {
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::Boolean),
                    value: Primitive::Boolean(true),
                })
            } else {
                Type::Boolean
            }
        }
        // TODO: suppot `typeof operator``
        _ => unimplemented!("Unsupported unary operator: {:?}", unary.op),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_expression;
    use swc_ecma_ast::*;

    fn should_synth(env: &Env, case: Vec<(&str, Type)>) {
        for (input, expected) in case {
            let expr = parse_expression(input);
            let _type = synth(&env, &expr);
            assert_eq!(_type, expected);
        }
    }

    #[test]
    fn should_parse_literal_to_type() {
        let cases = vec![
            (
                Lit::Null(Null {
                    span: Default::default(),
                }),
                Type::Null,
            ),
            (
                Lit::Bool(Bool {
                    span: Default::default(),
                    value: true,
                }),
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::Boolean),
                    value: Primitive::Boolean(true),
                }),
            ),
            (
                Lit::Bool(Bool {
                    span: Default::default(),
                    value: false,
                }),
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::Boolean),
                    value: Primitive::Boolean(false),
                }),
            ),
            (
                Lit::Num(Number {
                    span: Default::default(),
                    value: 7.0,
                    raw: None,
                }),
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::Number),
                    value: Primitive::Number(7),
                }),
            ),
            (
                Lit::Str(Str {
                    span: Default::default(),
                    value: "hello".into(),
                    raw: None,
                }),
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::String),
                    value: Primitive::String("hello".to_string()),
                }),
            ),
        ];

        for (lit, expected) in cases {
            assert_eq!(synth_literal(&lit), expected);
        }
    }

    #[test]
    fn should_parse_object_to_type() {
        let cases = vec![(
            "{ n: 9, b: true, s: 'hello' }",
            Type::Object(vec![
                ObjectProp::new(
                    "n",
                    Type::Singleton(SingletonProp::new(Type::Number, Primitive::Number(9))),
                ),
                ObjectProp::new(
                    "b",
                    Type::Singleton(SingletonProp::new(Type::Boolean, Primitive::Boolean(true))),
                ),
                ObjectProp::new(
                    "s",
                    Type::Singleton(SingletonProp::new(
                        Type::String,
                        Primitive::String("hello".to_string()),
                    )),
                ),
            ]),
        )];

        should_synth(&Env::new(), cases);
    }

    #[test]
    fn should_synth_object() {
        let env = Env::new().set("x", Type::Number);

        let cases = vec![(
            "{ n: 9, b: x }",
            Type::Object(vec![
                ObjectProp::new(
                    "n",
                    Type::Singleton(SingletonProp::new(Type::Number, Primitive::Number(9))),
                ),
                ObjectProp::new("b", Type::Number),
            ]),
        )];

        should_synth(&env, cases);
    }

    #[test]
    fn should_synth_function() {
        let env = Env::new().set("x", Type::Number);

        let cases = vec![
            (
                "(x: number, y: string) => ({ x: x, y: y })",
                Type::Function(FunctionProp::new(
                    vec![Type::Number, Type::String],
                    Type::Object(vec![
                        ObjectProp::new("x", Type::Number),
                        ObjectProp::new("y", Type::String),
                    ]),
                )),
            ),
            (
                "(x: number, y: string) => x",
                Type::Function(FunctionProp::new(
                    vec![Type::Number, Type::String],
                    Type::Number,
                )),
            ),
        ];

        should_synth(&env, cases);
    }

    #[test]
    fn should_synth_call() {
        let env = Env::new().set(
            "f",
            Type::Function(FunctionProp::new(
                vec![Type::Number, Type::Number],
                Type::Number,
            )),
        );

        let cases = vec![("f(7, 9)", Type::Number)];

        should_synth(&env, cases);
    }

    #[test]
    fn should_synth_bin_add() {
        let env = Env::new().set("x", Type::Number);
        let cases = vec![("x + 7", Type::Number)];

        should_synth(&env, cases);
    }

    #[test]
    fn should_synth_bin_eqeqeq() {
        let env = Env::new().set("x", Type::Number);

        let cases = vec![
            ("x === 7", Type::Boolean),
            (
                "'hello' === 'hello'",
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::Boolean),
                    value: Primitive::Boolean(true),
                }),
            ),
        ];

        for (input, expected) in cases {
            let expr = parse_expression(input);
            let _type = synth(&env, &expr);
            assert_eq!(_type, expected);
        }
    }

    #[test]
    fn should_synth_bin_noteqeq() {
        let env = Env::new().set("x", Type::Number);

        let cases = vec![
            ("x !== 7", Type::Boolean),
            (
                "'hello' !== 'hello'",
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::Boolean),
                    value: Primitive::Boolean(false),
                }),
            ),
        ];

        should_synth(&env, cases);
    }

    #[test]
    fn should_synth_bin_logical_and() {
        let env = Env::new().set("x", Type::String);

        let cases = vec![
            (
                "x && true",
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::Boolean),
                    value: Primitive::Boolean(true),
                }),
            ),
            ("'hello' && x", Type::String),
        ];

        should_synth(&env, cases);
    }

    #[test]
    fn should_synth_bin_logical_or() {
        let env = Env::new().set("x", Type::Boolean);

        let cases = vec![
            ("x || false", Type::Boolean),
            (
                "true || x",
                Type::Singleton(SingletonProp {
                    base: Box::new(Type::Boolean),
                    value: Primitive::Boolean(true),
                }),
            ),
        ];

        should_synth(&env, cases);
    }

    #[test]
    fn should_synth_member() {
        let env = Env::new().set(
            "x",
            Type::Object(vec![
                ObjectProp::new("n", Type::Number),
                ObjectProp::new("b", Type::Boolean),
                ObjectProp::new("s", Type::String),
            ]),
        );

        let cases = vec![
            ("x.n", Type::Number),
            ("x.b", Type::Boolean),
            ("x.s", Type::String),
        ];

        should_synth(&env, cases);
    }

    #[test]
    fn should_handle_union_type() {
        let env = Env::new().set(
            "x",
            Type::Union(vec![
                Type::Object(vec![ObjectProp::new("b", Type::Boolean)]),
                Type::Object(vec![ObjectProp::new("b", Type::String)]),
            ]),
        );

        let cases = vec![("x.b", Type::Union(vec![Type::Boolean, Type::String]))];

        should_synth(&env, cases);
    }
}
