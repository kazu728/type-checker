use crate::env::Env;
use crate::types::{
    is_subtype, of_ts_type, FunctionProperty, ObjectProperty, PrimitiveType, SingletonProperty,
    Type,
};
use swc_ecma_ast::{
    ArrowExpr, BinaryOp, BlockStmtOrExpr, CallExpr, Callee, Expr, Ident, Lit, ObjectLit, Pat, Prop,
    PropName, PropOrSpread, Stmt,
};

pub fn synth(env: &Env, expr: &Expr) -> Type {
    match expr {
        Expr::Ident(ident) => synth_identifier(env, ident),
        Expr::Lit(lit) => synth_literal(lit),
        Expr::Object(obj) => synth_object(env, obj),
        Expr::Arrow(arrow) => synth_function(env, arrow),
        Expr::Call(call) => synth_call(&env, call),
        Expr::Paren(paren) => synth(env, &paren.expr),
        Expr::Bin(bin) => synth_bin(env, bin),
        _ => unimplemented!("Unsupported expression: {:?}", expr),
    }
}

fn synth_identifier(env: &Env, ident: &Ident) -> Type {
    let name = ident.sym.to_string();
    let _type = env.get(&name).unwrap_or_else(|| {
        eprintln!("Unbound variable: {}", name);
        std::process::exit(1);
    });
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
    Type::Singleton(SingletonProperty {
        base: Box::new(Type::Boolean),
        value: PrimitiveType::Boolean(ast.value),
    })
}

fn synth_number(ast: &swc_ecma_ast::Number) -> Type {
    Type::Singleton(SingletonProperty {
        base: Box::new(Type::Number),
        value: PrimitiveType::Number(ast.value as usize),
    })
}

fn synth_string(ast: &swc_ecma_ast::Str) -> Type {
    Type::Singleton(SingletonProperty {
        base: Box::new(Type::String),
        value: PrimitiveType::String(ast.value.to_string()),
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
                    Some(ObjectProperty {
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

fn synth_function(env: &Env, arrow: &ArrowExpr) -> Type {
    let mut bindings = Vec::new();
    let mut new_env = env.clone();

    for param in &arrow.params {
        if let Pat::Ident(ident) = param {
            let name = ident.id.sym.to_string();

            let _type = match param.clone().ident().unwrap().type_ann {
                Some(ts_type) => of_ts_type(*ts_type),
                None => Type::Null,
            };

            bindings.push(ObjectProperty {
                name: name.clone(),
                _type: _type.clone(),
            });

            new_env.set(&name, _type);
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

    Type::Function(FunctionProperty {
        args: bindings.into_iter().map(|p| p._type).collect(),
        ret: Box::new(ret),
    })
}

fn synth_call(env: &Env, call: &CallExpr) -> Type {
    let callee = match &call.callee {
        Callee::Expr(expr) => synth(env, &*expr),
        _ => panic!("Unexpected callee type: {:?}", call.callee),
    };

    let args = call
        .args
        .iter()
        .map(|arg| synth(env, &arg.expr))
        .collect::<Vec<Type>>();

    match callee {
        Type::Function(FunctionProperty {
            args: expected_args,
            ret,
        }) => {
            if args.len() != expected_args.len() {
                panic!(
                    "Number of arguments does not match: expected {} but got {}",
                    expected_args.len(),
                    args.len()
                );
            }

            for (i, (expected, actual)) in expected_args.iter().zip(args.iter()).enumerate() {
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
            (Type::Singleton(left), Type::Singleton(right)) => Type::Singleton(SingletonProperty {
                base: Box::new(Type::Boolean),
                value: PrimitiveType::Boolean(left.value == right.value),
            }),
            _ => Type::Boolean,
        },
        BinaryOp::NotEqEq => match (left, right) {
            (Type::Singleton(left), Type::Singleton(right)) => Type::Singleton(SingletonProperty {
                base: Box::new(Type::Boolean),
                value: PrimitiveType::Boolean(left.value != right.value),
            }),
            _ => Type::Boolean,
        },
        BinaryOp::Add => {
            if is_subtype(&left, &Type::Number) && is_subtype(&right, &Type::Number) {
                match (&left, &right) {
                    (Type::Singleton(left), Type::Singleton(right)) => {
                        Type::Singleton(SingletonProperty {
                            base: Box::new(Type::Number),
                            value: PrimitiveType::Number(match (&left.value, &right.value) {
                                (PrimitiveType::Number(left), PrimitiveType::Number(right)) => {
                                    left + right
                                }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_expression;
    use swc_ecma_ast::*;

    #[test]
    fn should_parse_literal_to_type() {
        let lit_null = Lit::Null(Null {
            span: Default::default(),
        });
        assert_eq!(synth_literal(&lit_null), Type::Null);

        let lit_true = Lit::Bool(Bool {
            span: Default::default(),
            value: true,
        });
        assert_eq!(
            synth_literal(&lit_true),
            Type::Singleton(SingletonProperty {
                base: Box::new(Type::Boolean),
                value: PrimitiveType::Boolean(true)
            })
        );

        let lit_false = Lit::Bool(Bool {
            span: Default::default(),
            value: false,
        });
        assert_eq!(
            synth_literal(&lit_false),
            Type::Singleton(SingletonProperty {
                base: Box::new(Type::Boolean),
                value: PrimitiveType::Boolean(false)
            })
        );

        let lit_number = Lit::Num(Number {
            span: Default::default(),
            value: 7.0,
            raw: None,
        });
        assert_eq!(
            synth_literal(&lit_number),
            Type::Singleton(SingletonProperty {
                base: Box::new(Type::Number),
                value: PrimitiveType::Number(7)
            })
        );

        let lit_string = Lit::Str(Str {
            span: Default::default(),
            value: "hello".into(),
            raw: None,
        });
        assert_eq!(
            synth_literal(&lit_string),
            Type::Singleton(SingletonProperty {
                base: Box::new(Type::String),
                value: PrimitiveType::String("hello".to_string())
            })
        );
    }

    #[test]
    fn should_parse_object_to_type() {
        let input = "{ n: 9, b: true, s: 'hello' }";
        let expr = parse_expression(input);
        if let Expr::Object(obj) = *expr {
            let obj_type = synth_object(&Env::new(), &obj);
            assert_eq!(
                obj_type,
                Type::Object(vec![
                    ObjectProperty {
                        name: "n".to_string(),
                        _type: Type::Singleton(SingletonProperty {
                            base: Box::new(Type::Number),
                            value: PrimitiveType::Number(9)
                        })
                    },
                    ObjectProperty {
                        name: "b".to_string(),
                        _type: Type::Singleton(SingletonProperty {
                            base: Box::new(Type::Boolean),
                            value: PrimitiveType::Boolean(true)
                        })
                    },
                    ObjectProperty {
                        name: "s".to_string(),
                        _type: Type::Singleton(SingletonProperty {
                            base: Box::new(Type::String),
                            value: PrimitiveType::String("hello".to_string())
                        })
                    }
                ])
            );
        } else {
            panic!("Expected object literal");
        }
    }

    #[test]
    fn should_synth_object() {
        let env = Env::new().set("x", Type::Number);

        let expr = parse_expression("{ n: 9, b: x }");
        let _type = synth(&env, &expr);
        assert_eq!(
            _type,
            Type::Object(vec![
                ObjectProperty {
                    name: "n".to_string(),
                    _type: Type::Singleton(SingletonProperty {
                        base: Box::new(Type::Number),
                        value: PrimitiveType::Number(9)
                    })
                },
                ObjectProperty {
                    name: "b".to_string(),
                    _type: Type::Number
                },
            ])
        );
    }

    #[test]
    fn should_synth_function() {
        let env = Env::new().set("x", Type::Number);
        let expr = parse_expression("(x: number, y: string) => ({ x: x, y: y })");
        let _type = synth(&env, &expr);
        assert_eq!(
            _type,
            Type::Function(FunctionProperty {
                args: vec![Type::Number, Type::String],
                ret: Box::new(Type::Object(vec![
                    ObjectProperty {
                        name: "x".to_string(),
                        _type: Type::Number
                    },
                    ObjectProperty {
                        name: "y".to_string(),
                        _type: Type::String
                    },
                ]))
            })
        );
    }

    #[test]
    fn should_synth_call() {
        let env = Env::new().set(
            "f",
            Type::Function(FunctionProperty {
                args: vec![Type::Number, Type::Number],
                ret: Box::new(Type::Number),
            }),
        );

        let expr = parse_expression("f(7, 9)");
        let _type = synth(&env, &expr);
        assert_eq!(_type, Type::Number);
    }

    #[test]
    fn should_synth_bin_add() {
        let env = Env::new().set("x", Type::Number);

        let expr = parse_expression("x + 7");
        let _type = synth(&env, &expr);
        assert_eq!(_type, Type::Number);
    }

    #[test]
    fn should_synth_bin_eqeqeq() {
        let env = Env::new().set("x", Type::Number);

        let expr = parse_expression("x === 7");
        let _type = synth(&env, &expr);
        assert_eq!(_type, Type::Boolean);

        let expr = parse_expression("'hello' === 'hello'");
        let _type = synth(&env, &expr);
        assert_eq!(
            _type,
            Type::Singleton(SingletonProperty {
                base: Box::new(Type::Boolean),
                value: PrimitiveType::Boolean(true)
            })
        );
    }

    #[test]
    fn should_synth_bin_noteqeq() {
        let env = Env::new().set("x", Type::Number);

        let expr = parse_expression("x !== 7");
        let _type = synth(&env, &expr);
        assert_eq!(_type, Type::Boolean);

        let expr = parse_expression("'hello' !== 'hello'");
        let _type = synth(&env, &expr);
        assert_eq!(
            _type,
            Type::Singleton(SingletonProperty {
                base: Box::new(Type::Boolean),
                value: PrimitiveType::Boolean(false)
            })
        );
    }

    #[test]
    fn should_synth_bin_logical_and() {
        let env = Env::new().set("x", Type::String);

        let expr = parse_expression("x && true");
        let _type = synth(&env, &expr);
        assert_eq!(
            _type,
            Type::Singleton(SingletonProperty {
                base: Box::new(Type::Boolean),
                value: PrimitiveType::Boolean(true)
            })
        );

        let expr = parse_expression("'hello' && x");
        let _type = synth(&env, &expr);
        assert_eq!(_type, Type::String)
    }

    #[test]
    fn should_synth_bin_logical_or() {
        let env = Env::new().set("x", Type::Boolean);

        let expr = parse_expression("x || false");
        let _type = synth(&env, &expr);
        assert_eq!(_type, Type::Boolean);

        let expr = parse_expression("true || x");
        let _type = synth(&env, &expr);
        assert_eq!(
            _type,
            Type::Singleton(SingletonProperty {
                base: Box::new(Type::Boolean),
                value: PrimitiveType::Boolean(true)
            })
        )
    }
}
