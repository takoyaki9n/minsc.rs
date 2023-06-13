use std::{fmt, rc::Rc};

use crate::value::{self, Value};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionData {
    Nil,
    Atom(Value),
    Cons(Expression, Expression),
}
pub type Expression = Rc<ExpressionData>;

pub fn nil() -> Expression {
    Rc::new(ExpressionData::Nil)
}

pub fn atom(v: Value) -> Expression {
    Rc::new(ExpressionData::Atom(v))
}

pub fn undef() -> Expression {
    atom(value::undef())
}

pub fn bool(b: bool) -> Expression {
    atom(value::bool(b))
}

pub fn int(n: i64) -> Expression {
    atom(value::int(n))
}

pub fn symbol<S: Into<String>>(s: S) -> Expression {
    atom(value::symbol(s))
}

pub fn built_in_proc<S: Into<String>>(
    name: S,
    proc: fn(Vec<Expression>) -> Result<Expression, String>,
) -> Expression {
    atom(value::built_in_proc(name, proc))
}

pub fn cons(car: Expression, cdr: Expression) -> Expression {
    Rc::new(ExpressionData::Cons(car.clone(), cdr.clone()))
}

pub fn list(exprs: Vec<Expression>) -> Expression {
    exprs
        .into_iter()
        .rfold(nil(), |list, expr| cons(expr, list))
}

pub fn to_vec(mut expr: Expression) -> Option<Vec<Expression>> {
    let mut exprs = Vec::new();

    while let ExpressionData::Cons(car, cdr) = expr.as_ref() {
        exprs.push(car.clone());
        expr = cdr.clone();
    }

    match *expr {
        ExpressionData::Nil => Some(exprs),
        _ => None,
    }
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
            fmt_expression(&cdr, f, true)
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
        assert_eq!(format!("{}", nil()), "()");
        assert_eq!(format!("{}", symbol("x")), "x");
        assert_eq!(format!("{}", cons(nil(), bool(true))), "(() . #t)");
        assert_eq!(format!("{}", list(vec![int(1), int(2)])), "(1 2)");
        let expr = cons(int(1), cons(int(2), int(3)));
        assert_eq!(format!("{}", expr), "(1 2 . 3)");
        let expr = list(vec![
            symbol("let"),
            list(vec![list(vec![symbol("a"), int(2)])]),
            list(vec![symbol("-"), symbol("a")]),
        ]);
        assert_eq!(format!("{}", expr), "(let ((a 2)) (- a))");
    }
}
