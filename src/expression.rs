use std::fmt;

use crate::value::{self, Value};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Nil,
    Atom(Value),
    Cons(Box<Self>, Box<Self>),
}

impl Expression {
    pub fn flatten(self) -> Option<Vec<Self>> {
        let mut exprs = Vec::new();
        let mut expr = self;

        while let Self::Cons(car, cdr) = expr {
            exprs.push(*car);
            expr = *cdr;
        }

        match expr {
            Self::Nil => Some(exprs),
            _ => None,
        }
    }
}

pub fn nil() -> Expression {
    Expression::Nil
}

pub fn atom(v: Value) -> Expression {
    Expression::Atom(v)
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

pub fn cons(car: Expression, cdr: Expression) -> Expression {
    Expression::Cons(Box::new(car), Box::new(cdr))
}

pub fn list(exprs: Vec<Expression>) -> Expression {
    exprs
        .into_iter()
        .rfold(nil(), |list, expr| cons(expr, list))
}

fn fmt_expression(expr: &Expression, f: &mut fmt::Formatter<'_>, is_cdr: bool) -> fmt::Result {
    match expr {
        Expression::Nil if is_cdr => write!(f, ")"),
        Expression::Nil => write!(f, "()"),
        Expression::Atom(s) if is_cdr => write!(f, ". {})", s),
        Expression::Atom(s) => write!(f, "{}", s),
        Expression::Cons(car, cdr) => {
            if !is_cdr {
                write!(f, "(")?;
            }
            fmt_expression(&car, f, false)?;
            if **cdr != Expression::Nil {
                write!(f, " ")?;
            }
            fmt_expression(&cdr, f, true)
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_expression(self, f, false)
    }
}

#[cfg(test)]
mod tests {
    use super::{cons, int, list, nil, symbol, Expression};

    fn simple_list_elems() -> Vec<Expression> {
        vec![int(1), int(2)]
    }

    fn simple_list() -> Expression {
        list(simple_list_elems())
    }

    fn incomplete_list() -> Expression {
        cons(int(1), cons(int(2), int(3)))
    }

    fn let_expression_elems() -> Vec<Expression> {
        vec![
            symbol("let"),
            list(vec![list(vec![symbol("a"), int(2)])]),
            list(vec![symbol("-"), symbol("a")]),
        ]
    }

    fn let_expression() -> Expression {
        list(let_expression_elems())
    }

    macro_rules! flatten_tests {
        ($($name: ident: $case: expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (expected, input) = $case;
                    assert_eq!(expected, input.flatten());
                }
            )*
        }
    }
    flatten_tests! {
        flatten_nil: (Some(vec![]), nil()),
        flatten_atom: (None, symbol("x")),
        flatten_simple_list: (Some(simple_list_elems()), simple_list()),
        flatten_incomplete_list: (None, incomplete_list()),
        flatten_let_expression: (Some(let_expression_elems()), let_expression()),
    }

    macro_rules! display_tests {
        ($($name: ident: $case: expr,)*) => {
            $(
                #[test]
                fn $name() {
                    let (expected, input) = $case;
                    assert_eq!(expected, format!("{}", input));
                }
            )*
        }
    }
    display_tests! {
        display_nil: ("()", nil()),
        display_atom: ("x", symbol("x")),
        display_cons: ("(() . 0)", cons(nil(), int(0))),
        display_simple_list: ("(1 2)", simple_list()),
        display_incomplete_list: ("(1 2 . 3)", incomplete_list()),
        display_let_expression: ("(let ((a 2)) (- a))", let_expression()),
    }
}
