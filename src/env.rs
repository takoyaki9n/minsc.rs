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
    pub fn new() -> Self {
        Self {
            outer: None,
            frame: RefCell::new(Frame::new()),
        }
    }

    pub fn get<S: Into<String>>(&self, name: S) -> Option<Expression> {
        let name = name.into();
        self.frame.borrow().get(&name).map_or_else(
            || self.outer.as_ref().and_then(|env| env.get(name)),
            |value| Some(value.clone()),
        )
    }

    pub fn set<S: Into<String>>(&self, name: S, value: Expression) {
        self.frame.borrow_mut().insert(name.into(), value);
    }
}

pub fn top() -> Env {
    Rc::new(EnvData::new())
}

pub fn extend(outer: Env) -> Env {
    Rc::new(EnvData {
        outer: Some(outer),
        frame: RefCell::new(Frame::new()),
    })
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{
        env::{extend, top},
        expression::{bool, int},
    };

    #[test]
    fn test() {
        let env = top();
        env.set("x", int(32));
        if let Some(value) = env.get("x") {
            env.set("y", value)
        }
        assert_eq!(env.get("x"), Some(int(32)));
        assert_eq!(env.get("y"), Some(int(32)));

        let extended = extend(Rc::clone(&env));
        extended.set("x", bool(true));
        assert_eq!(extended.get("x"), Some(bool(true)));
        assert_eq!(extended.get("y"), Some(int(32)));

        assert_eq!(env.get("x"), Some(int(32)));
        assert_eq!(env.get("y"), Some(int(32)));
    }
}
