use std::{fmt, rc::Rc};

use crate::{
    env::Env,
    value::Value::{self, Bool, BuiltInProc, Closure, Int, SpecialForm, Symbol, Undef},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionInner {
    Nil,
    Atom(Value),
    Cons(Expression, Expression),
}
use ExpressionInner::{Atom, Cons, Nil};
pub type Expression = Rc<ExpressionInner>;

pub trait ExpressionConverter {
    fn as_number(&self) -> Result<i64, Expression>;
    fn as_symbol(&self) -> Result<String, Expression>;
    fn as_vec(&self) -> Result<Vec<Expression>, Expression>;
}

impl ExpressionConverter for Expression {
    fn as_number(&self) -> Result<i64, Expression> {
        match self.as_ref() {
            Atom(Int(n)) => Ok(*n),
            _ => Err(Rc::clone(self)),
        }
    }

    fn as_symbol(&self) -> Result<String, Expression> {
        match self.as_ref() {
            Atom(Symbol(name)) => Ok(name.to_string()),
            _ => Err(Rc::clone(self)),
        }
    }

    fn as_vec(&self) -> Result<Vec<Expression>, Expression> {
        let mut exprs = vec![];
        let mut expr = self;
        while let Cons(car, cdr) = expr.as_ref() {
            exprs.push(Rc::clone(car));
            expr = cdr;
        }

        match expr.as_ref() {
            Nil => Ok(exprs),
            _ => Err(Rc::clone(expr)),
        }
    }
}

pub fn nil() -> Expression {
    Rc::new(Nil)
}

pub fn atom(v: Value) -> Expression {
    Rc::new(Atom(v))
}

pub fn undef() -> Expression {
    atom(Undef)
}

pub fn bool(b: bool) -> Expression {
    atom(Bool(b))
}

pub fn int(n: i64) -> Expression {
    atom(Int(n))
}

pub fn symbol(s: impl Into<String>) -> Expression {
    atom(Symbol(s.into()))
}

pub fn special_form(
    name: impl Into<String>,
    eval: fn(&[Expression], &Env) -> Result<Expression, String>,
) -> Expression {
    atom(SpecialForm {
        name: name.into(),
        eval,
    })
}

pub fn built_in_proc(
    name: impl Into<String>,
    proc: fn(&[Expression]) -> Result<Expression, String>,
) -> Expression {
    atom(BuiltInProc {
        name: name.into(),
        proc,
    })
}

pub fn closure(params: Vec<String>, body: Vec<Expression>, env: Env) -> Expression {
    atom(Closure { params, body, env })
}

pub fn cons(car: Expression, cdr: Expression) -> Expression {
    Rc::new(Cons(car, cdr))
}

#[cfg(test)]
pub fn list(exprs: Vec<Expression>) -> Expression {
    exprs
        .into_iter()
        .rfold(nil(), |list, expr| cons(expr, list))
}

fn fmt_expression(expr: &ExpressionInner, f: &mut fmt::Formatter<'_>, is_cdr: bool) -> fmt::Result {
    match expr {
        Nil if is_cdr => write!(f, ")"),
        Nil => write!(f, "()"),
        Atom(s) if is_cdr => write!(f, ". {})", s),
        Atom(s) => write!(f, "{}", s),
        Cons(car, cdr) => {
            if !is_cdr {
                write!(f, "(")?;
            }
            fmt_expression(car.as_ref(), f, false)?;
            if *cdr.as_ref() != Nil {
                write!(f, " ")?;
            }
            fmt_expression(cdr.as_ref(), f, true)
        }
    }
}

impl fmt::Display for ExpressionInner {
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
