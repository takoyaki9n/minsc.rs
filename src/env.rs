use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::value::Value;

type Frame = HashMap<String, Value>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnvData {
    outer: Option<Env>,
    frame: RefCell<Frame>,
}
pub type Env = Rc<EnvData>;

impl EnvData {
    pub fn new() -> Self {
        Self {
            outer: None,
            frame: RefCell::new(Frame::new()),
        }
    }

    pub fn get<S: Into<String>>(&self, name: S) -> Option<Value> {
        let name = name.into();
        self.frame.borrow().get(&name).map_or_else(
            || self.outer.as_ref().and_then(|env| env.get(name)),
            |value| Some(value.clone()),
        )
    }

    pub fn set<S: Into<String>>(&self, name: S, value: Value) {
        self.frame.borrow_mut().insert(name.into(), value.clone());
    }
}

pub fn top() -> Env {
    Rc::new(EnvData::new())
}

pub fn extend(outer: Env) -> Env {
    Rc::new(EnvData {
        outer: Some(outer.clone()),
        frame: RefCell::new(Frame::new()),
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        env::{extend, top},
        value::{bool, int},
    };

    #[test]
    fn test() {
        let env = top();
        env.set("x", int(32));
        env.get("x").map(|value| env.set("y", value));
        assert_eq!(env.get("x"), Some(int(32)));
        assert_eq!(env.get("y"), Some(int(32)));

        let extended = extend(env.clone());
        extended.set("x", bool(true));
        assert_eq!(extended.get("x"), Some(bool(true)));
        assert_eq!(extended.get("y"), Some(int(32)));

        assert_eq!(env.get("x"), Some(int(32)));
        assert_eq!(env.get("y"), Some(int(32)));
    }
}
