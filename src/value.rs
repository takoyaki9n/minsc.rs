use std::{fmt, rc::Rc};

use crate::{env::Env, expression::Expression};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueData {
    Undef,
    Bool(bool),
    Int(i64),
    Symbol(String),
    BuiltInProc {
        name: String,
        proc: fn(Vec<Value>) -> Result<Value, String>,
    },
    Closure {
        params: Vec<String>,
        body: Vec<Expression>,
        env: Env,
    },
}
pub type Value = Rc<ValueData>;

pub fn undef() -> Value {
    Rc::new(ValueData::Undef)
}

pub fn bool(b: bool) -> Value {
    Rc::new(ValueData::Bool(b))
}

pub fn int(n: i64) -> Value {
    Rc::new(ValueData::Int(n))
}

pub fn symbol<S: Into<String>>(s: S) -> Value {
    Rc::new(ValueData::Symbol(s.into()))
}

pub fn built_in_proc<S: Into<String>>(
    name: S,
    proc: fn(Vec<Value>) -> Result<Value, String>,
) -> Value {
    Rc::new(ValueData::BuiltInProc {
        name: name.into(),
        proc,
    })
}

pub fn closure(params: Vec<String>, body: Vec<Expression>, env: &Env) -> Value {
    Rc::new(ValueData::Closure {
        params,
        body,
        env: env.clone(),
    })
}

impl fmt::Display for ValueData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undef => write!(f, "<Undef>"),
            Self::Bool(b) if *b => write!(f, "#t"),
            Self::Bool(_) => write!(f, "#f"),
            Self::Int(n) => write!(f, "{}", n),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::BuiltInProc { name, .. } => write!(f, "<Built-In-Proc: {}>", name),
            Self::Closure { params, .. } => write!(f, "<Closure ({})>", params.join(", ")),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env,
        expression::nil,
        value::{built_in_proc, closure, undef},
    };

    use super::{bool, int, symbol, Value};

    fn proc_foo(args: Vec<Value>) -> Result<Value, String> {
        Ok(int(args.len().try_into().unwrap()))
    }

    fn closure_bar() -> Value {
        closure(vec![format!("x"), format!("y")], vec![nil()], &env::top())
    }

    #[test]
    fn display_test() {
        assert_eq!(format!("{}", undef()), "<Undef>");
        assert_eq!(format!("{}", bool(true)), "#t");
        assert_eq!(format!("{}", bool(false)), "#f");
        assert_eq!(format!("{}", int(-1234)), "-1234");
        assert_eq!(format!("{}", symbol("x")), "x");
        let foo = built_in_proc("foo", proc_foo);
        assert_eq!(format!("{}", foo), "<Built-In-Proc: foo>");
        assert_eq!(format!("{}", closure_bar()), "<Closure (x, y)>");
    }
}
