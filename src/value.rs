use std::{fmt, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueData {
    Bool(bool),
    Int(i64),
    Symbol(String),
    BuiltInProc(String, fn(Vec<Value>) -> Result<Value, String>),
}
pub type Value = Rc<ValueData>;

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
    Rc::new(ValueData::BuiltInProc(name.into(), proc))
}

impl fmt::Display for ValueData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(b) if *b => write!(f, "#t"),
            Self::Bool(_) => write!(f, "#f"),
            Self::Int(n) => write!(f, "{}", n),
            Self::Symbol(s) => write!(f, "{}", s),
            Self::BuiltInProc(name, _) => write!(f, "<Built-In-Proc: {}>", name),
        }
    }
}
