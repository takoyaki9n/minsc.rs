use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Symbol(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(b) if *b => write!(f, "#t"),
            Self::Bool(_) => write!(f, "#f"),
            Self::Int(n) => write!(f, "{}", n),
            Self::Symbol(s) => write!(f, "{}", s),
        }
    }
}
