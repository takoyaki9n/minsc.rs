use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::value::Value;

pub struct Env {
    outer: Option<Rc<Env>>,
    frame: RefCell<HashMap<String, Rc<Value>>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            outer: None,
            frame: RefCell::new(HashMap::new()),
        }
    }

    pub fn extend(outer: Rc<Env>) -> Env {
        Env {
            outer: Some(outer.clone()),
            frame: RefCell::new(HashMap::new()),
        }
    }

    pub fn get<S: Into<String>>(&self, name: S) -> Option<Rc<Value>> {
        let name = name.into();
        self.frame.borrow().get(&name).map_or_else(
            || self.outer.as_ref().and_then(|env| env.get(name)),
            |value| Some(value.clone()),
        )
    }

    pub fn set<S: Into<String>>(&self, name: S, value: Rc<Value>) {
        self.frame.borrow_mut().insert(name.into(), value.clone());
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::value::{bool, int};

    use super::Env;

    #[test]
    fn test() {
        let env = Rc::new(Env::new());
        env.set("x", Rc::new(int(32)));
        env.get("x").map(|value| env.set("y", value));
        assert_eq!(env.get("x"), Some(Rc::new(int(32))));
        assert_eq!(env.get("y"), Some(Rc::new(int(32))));

        let extended = Env::extend(env.clone());
        extended.set("x", Rc::new(bool(true)));
        assert_eq!(extended.get("x"), Some(Rc::new(bool(true))));
        assert_eq!(extended.get("y"), Some(Rc::new(int(32))));
    }
}
