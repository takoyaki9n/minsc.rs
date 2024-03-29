use std::fmt;

use crate::{env::Env, expression::Expression};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Value {
    Undef,
    Bool(bool),
    Int(i64),
    Symbol(String),
    SpecialForm {
        name: String,
        eval: fn(&[Expression], &Env) -> Result<Expression, String>,
    },
    BuiltInProc {
        name: String,
        proc: fn(&[Expression]) -> Result<Expression, String>,
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
            Self::SpecialForm { name, .. } => write!(f, "<Special-Form: ({})>", name),
            Self::BuiltInProc { name, .. } => write!(f, "<Built-In-Proc: ({})>", name),
            Self::Closure { params, .. } => write!(f, "<Closure ({})>", params.join(", ")),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        env::{Env, EnvMaker},
        expression::nil,
    };

    use super::Value::{Bool, BuiltInProc, Closure, Int, SpecialForm, Symbol, Undef};

    #[test]
    fn display_test() {
        let cases = vec![
            (Undef, "<Undef>"),
            (Bool(true), "#t"),
            (Bool(false), "#f"),
            (Int(-1234), "-1234"),
            (Symbol("x".into()), "x"),
            (
                SpecialForm {
                    name: "foo".to_string(),
                    eval: |_, _| Ok(nil()),
                },
                "<Special-Form: (foo)>",
            ),
            (
                BuiltInProc {
                    name: "bar".to_string(),
                    proc: |_args| Ok(nil()),
                },
                "<Built-In-Proc: (bar)>",
            ),
            (
                Closure {
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
