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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Undef => write!(f, "<Undef>"),
            Self::Bool(b) if *b => write!(f, "#t"),
            Self::Bool(_) => write!(f, "#f"),
            Self::Int(n) => write!(f, "{}", n),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::BuiltInProc { name, .. } => write!(f, "<Built-In-Proc: ({})>", name),
            Self::Closure { params, .. } => write!(f, "<Closure ({})>", params.join(", ")),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{env::{Env, EnvMaker}, expression::nil};

    use super::Value;

    #[test]
    fn display_test() {
        let cases = vec![
            (Value::Undef, "<Undef>"),
            (Value::Bool(true), "#t"),
            (Value::Bool(false), "#f"),
            (Value::Int(-1234), "-1234"),
            (Value::Symbol("x".into()), "x"),
            (
                Value::BuiltInProc {
                    name: "foo".into(),
                    proc: |_args| Ok(nil()),
                },
                "<Built-In-Proc: (foo)>",
            ),
            (
                Value::Closure {
                    params: vec![format!("x"), format!("y")],
                    body: vec![nil()],
                    env: Env::empty(),
                },
                "<Closure (x, y)>",
            ),
        ];
        for (input, expected) in cases {
            assert_eq!(format!("{}", input), expected);
        }
    }
}
