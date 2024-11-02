use crate::types::Type;
use std::collections::HashMap;

#[derive(Clone)]
pub struct Env {
    map: HashMap<String, Type>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            map: HashMap::new(),
        }
    }
    pub fn get(&self, name: &str) -> Option<&Type> {
        self.map.get(name)
    }

    pub fn set(&mut self, name: &str, _type: Type) -> Self {
        self.map.insert(name.to_string(), _type);
        self.clone()
    }

    pub fn entries(&self) -> Vec<(String, Type)> {
        self.map
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    }
}

#[test]
fn should_get_env() {
    let mut env = Env::new();
    env.set("x", Type::Number);
    assert_eq!(env.get("x"), Some(&Type::Number));
}

#[test]
fn should_set_env() {
    let mut env = Env::new();
    let env = env.set("y", Type::Boolean);
    assert_eq!(env.get("y"), Some(&Type::Boolean));
}

#[test]
fn should_get_entries() {
    let env = &mut Env::new();
    let env = &mut env.set("y", Type::Boolean).set("x", Type::Number);

    let mut entries = env.entries();
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    assert_eq!(
        entries,
        vec![
            ("x".to_string(), Type::Number),
            ("y".to_string(), Type::Boolean)
        ]
    );
}
