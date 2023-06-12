use std::fmt;

use crate::{env::Env, expression::Expression};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Undef,
    Bool(bool),
    Int(i64),
    Symbol(String),
    BuiltInProc {
        name: String,
        proc: fn(Vec<Expression>) -> Result<Expression, String>,
    },
    Closure {
        params: Vec<String>,
        body: Vec<Expression>,
        env: Env,
    },
}

pub fn undef() -> Value {
    Value::Undef
}

pub fn bool(b: bool) -> Value {
    Value::Bool(b)
}

pub fn int(n: i64) -> Value {
    Value::Int(n)
}

pub fn symbol<S: Into<String>>(s: S) -> Value {
    Value::Symbol(s.into())
}

pub fn built_in_proc<S: Into<String>>(
    name: S,
    proc: fn(Vec<Expression>) -> Result<Expression, String>,
) -> Value {
    Value::BuiltInProc {
        name: name.into(),
        proc,
    }
}

pub fn closure(params: Vec<String>, body: Vec<Expression>, env: &Env) -> Value {
    Value::Closure {
        params,
        body,
        env: env.clone(),
    }
}

impl fmt::Display for Value {
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
        expression::{nil, Expression},
    };

    use super::{bool, built_in_proc, closure, int, symbol, undef};

    fn proc_foo(args: Vec<Expression>) -> Result<Expression, String> {
        Ok(nil())
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
        let bar = closure(vec![format!("x"), format!("y")], vec![nil()], &env::top());
        assert_eq!(format!("{}", bar), "<Closure (x, y)>");
    }
}
