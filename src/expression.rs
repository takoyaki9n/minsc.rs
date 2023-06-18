use std::{fmt, rc::Rc};

use crate::{env::Env, value::Value};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionData {
    Nil,
    Atom(Value),
    Cons(Expression, Expression),
}
pub type Expression = Rc<ExpressionData>;

impl ExpressionData {
    pub fn as_vec(&self) -> Option<Vec<Expression>> {
        let mut exprs = vec![];
        let mut expr = self;
        while let Self::Cons(car, cdr) = expr {
            exprs.push(Rc::clone(car));

            expr = cdr.as_ref();
        }

        match expr {
            Self::Nil => Some(exprs),
            _ => None,
        }
    }
}

pub fn nil() -> Expression {
    Rc::new(ExpressionData::Nil)
}

pub fn atom(v: Value) -> Expression {
    Rc::new(ExpressionData::Atom(v))
}

pub fn undef() -> Expression {
    atom(Value::Undef)
}

pub fn bool(b: bool) -> Expression {
    atom(Value::Bool(b))
}

pub fn int(n: i64) -> Expression {
    atom(Value::Int(n))
}

pub fn symbol<S: Into<String>>(s: S) -> Expression {
    atom(Value::Symbol(s.into()))
}

pub fn built_in_proc<S: Into<String>>(
    name: S,
    proc: fn(Vec<Expression>) -> Result<Expression, String>,
) -> Expression {
    atom(Value::BuiltInProc {
        name: name.into(),
        proc,
    })
}

pub fn closure(params: Vec<String>, body: Vec<Expression>, env: Env) -> Expression {
    atom(Value::Closure { params, body, env })
}

pub fn cons(car: Expression, cdr: Expression) -> Expression {
    Rc::new(ExpressionData::Cons(car, cdr))
}

#[cfg(test)]
pub fn list(exprs: Vec<Expression>) -> Expression {
    exprs
        .into_iter()
        .rfold(nil(), |list, expr| cons(expr, list))
}

fn fmt_expression(expr: &ExpressionData, f: &mut fmt::Formatter<'_>, is_cdr: bool) -> fmt::Result {
    match expr {
        ExpressionData::Nil if is_cdr => write!(f, ")"),
        ExpressionData::Nil => write!(f, "()"),
        ExpressionData::Atom(s) if is_cdr => write!(f, ". {})", s),
        ExpressionData::Atom(s) => write!(f, "{}", s),
        ExpressionData::Cons(car, cdr) => {
            if !is_cdr {
                write!(f, "(")?;
            }
            fmt_expression(car.as_ref(), f, false)?;
            if *cdr.as_ref() != ExpressionData::Nil {
                write!(f, " ")?;
            }
            fmt_expression(cdr.as_ref(), f, true)
        }
    }
}

impl fmt::Display for ExpressionData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_expression(self, f, false)
    }
}

#[cfg(test)]
mod tests {
    use super::{bool, cons, int, list, nil, symbol};

    #[test]
    fn display_test() {
        let cases = vec![
            (nil(), "()"),
            (symbol("x"), "x"),
            (cons(nil(), bool(true)), "(() . #t)"),
            (list(vec![int(1), int(2)]), "(1 2)"),
            (cons(int(1), cons(int(2), int(3))), "(1 2 . 3)"),
            (
                list(vec![
                    symbol("let"),
                    list(vec![list(vec![symbol("a"), int(2)])]),
                    list(vec![symbol("-"), symbol("a")]),
                ]),
                "(let ((a 2)) (- a))",
            ),
        ];
        for (input, expected) in cases {
            assert_eq!(format!("{}", input), expected)
        }
    }
}
