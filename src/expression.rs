use std::fmt;

use crate::value::Value;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    Nil,
    Atom(Value),
    Cons(Box<Self>, Box<Self>),
}

impl Expression {
    pub fn nil() -> Self {
        Self::Nil
    }

    pub fn atom(v: Value) -> Self {
        Self::Atom(v)
    }

    pub fn bool(b: bool) -> Self {
        Self::atom(Value::Bool(b))
    }

    pub fn int(n: i64) -> Self {
        Self::atom(Value::Int(n))
    }

    pub fn symbol<S: Into<String>>(s: S) -> Self {
        Self::atom(Value::Symbol(s.into()))
    }

    pub fn cons(car: Self, cdr: Self) -> Self {
        Self::Cons(Box::new(car), Box::new(cdr))
    }

    pub fn list(exprs: Vec<Self>) -> Self {
        exprs
            .into_iter()
            .rfold(Self::nil(), |list, expr| Self::cons(expr, list))
    }

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
    use super::Expression;

    fn simple_list_elems() -> Vec<Expression> {
        vec![Expression::int(1), Expression::int(2)]
    }

    fn simple_list() -> Expression {
        Expression::list(simple_list_elems())
    }

    fn incomplete_list() -> Expression {
        Expression::cons(
            Expression::int(1),
            Expression::cons(Expression::int(2), Expression::int(3)),
        )
    }

    fn let_expression_elems() -> Vec<Expression> {
        vec![
            Expression::symbol("let"),
            Expression::list(vec![Expression::list(vec![
                Expression::symbol("a"),
                Expression::int(2),
            ])]),
            Expression::list(vec![Expression::symbol("-"), Expression::symbol("a")]),
        ]
    }

    fn let_expression() -> Expression {
        Expression::list(let_expression_elems())
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
        flatten_nil: (Some(vec![]), Expression::nil()),
        flatten_atom: (None, Expression::symbol("x")),
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
        display_nil: ("()", Expression::nil()),
        display_atom: ("x", Expression::symbol("x")),
        display_cons: ("(() . 0)", Expression::cons(Expression::nil(), Expression::int(0))),
        display_simple_list: ("(1 2)", simple_list()),
        display_incomplete_list: ("(1 2 . 3)", incomplete_list()),
        display_let_expression: ("(let ((a 2)) (- a))", let_expression()),
    }
}
