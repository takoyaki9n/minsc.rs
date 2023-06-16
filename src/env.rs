use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::expression::Expression;

type Frame = HashMap<String, Expression>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnvData {
    outer: Option<Env>,
    frame: RefCell<Frame>,
}
pub type Env = Rc<EnvData>;

impl EnvData {
    pub fn new(outer: Option<Env>, frame: Option<Frame>) -> Self {
        Self {
            outer,
            frame: RefCell::new(frame.unwrap_or_default()),
        }
    }

    pub fn get(&self, name: impl Into<String>) -> Option<Expression> {
        let name = name.into();
        self.frame.borrow().get(&name).map_or_else(
            || self.outer.as_ref().and_then(|env| env.get(name)),
            |value| Some(value.clone()),
        )
    }

    pub fn set(&self, name: impl Into<String>, value: Expression) {
        self.frame.borrow_mut().insert(name.into(), value);
    }
}

pub trait EnvMaker {
    fn empty() -> Env;
    fn extend(&self) -> Env;
}
impl EnvMaker for Env {
    fn empty() -> Env {
        Rc::new(EnvData::new(None, None))
    }

    fn extend(&self) -> Env {
        Rc::new(EnvData::new(Some(Rc::clone(self)), None))
    }
}

#[cfg(test)]
mod tests {
    use super::Env;
    use crate::{
        env::EnvMaker,
        expression::{bool, int},
    };

    #[test]
    fn test() {
        let env = Env::empty();
        env.set("x", int(32));
        if let Some(value) = env.get("x") {
            env.set("y", value)
        }
        assert_eq!(env.get("x"), Some(int(32)));
        assert_eq!(env.get("y"), Some(int(32)));

        let extended = env.extend();
        extended.set("x", bool(true));
        assert_eq!(extended.get("x"), Some(bool(true)));
        assert_eq!(extended.get("y"), Some(int(32)));

        assert_eq!(env.get("x"), Some(int(32)));
        assert_eq!(env.get("y"), Some(int(32)));
    }
}
