use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum SExpression {
    Nil,
    Atom(String),
    Cons(Box<SExpression>, Box<SExpression>),
}

impl SExpression {
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

pub fn nil() -> SExpression {
    SExpression::Nil
}

pub fn atom<S: Into<String>>(s: S) -> SExpression {
    SExpression::Atom(s.into())
}

pub fn cons(car: SExpression, cdr: SExpression) -> SExpression {
    SExpression::Cons(Box::new(car), Box::new(cdr))
}

pub fn list(exprs: Vec<SExpression>) -> SExpression {
    exprs
        .into_iter()
        .rfold(nil(), |list, expr| cons(expr, list))
}

fn fmt_expression(expr: &SExpression, f: &mut fmt::Formatter<'_>, is_cdr: bool) -> fmt::Result {
    match expr {
        SExpression::Nil if is_cdr => write!(f, ")"),
        SExpression::Nil => write!(f, "()"),
        SExpression::Atom(s) if is_cdr => write!(f, ". {})", s),
        SExpression::Atom(s) => write!(f, "{}", s),
        SExpression::Cons(car, cdr) => {
            if !is_cdr {
                write!(f, "(")?;
            }
            fmt_expression(&car, f, false)?;
            if **cdr != SExpression::Nil {
                write!(f, " ")?;
            }
            fmt_expression(&cdr, f, true)
        }
    }
}

impl fmt::Display for SExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_expression(self, f, false)
    }
}

#[cfg(test)]
mod tests {
    use super::{atom, cons, list, nil};

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
        flatten_atom: (None, atom("x")),
        flatten_simple_list: (Some(vec![atom("1"), atom("2")]), list(vec![atom("1"), atom("2")])),
        flatten_incomplete_list: (None, cons(atom("1"), cons(atom("2"), atom("3")))),
        flatten_nested_expression: (
            Some(vec![
                atom("let"),
                list(vec![list(vec![atom("a"), atom("2")])]),
                list(vec![atom("-"), atom("a")]),
            ]),
            list(vec![
                atom("let"),
                list(vec![list(vec![atom("a"), atom("2")])]),
                list(vec![atom("-"), atom("a")]),
            ])
        ),
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
        display_atom: ("x", atom("x")),
        display_cons: ("(() . 0)", cons(nil(), atom("0"))),
        display_simple_list: ("(1 2)", list(vec![atom("1"), atom("2")])),
        display_incomplete_list: ("(1 2 . 3)", cons(atom("1"), cons(atom("2"), atom("3")))),
        display_expression: (
            "(let ((a 2)) (- a))",
            list(vec![
                atom("let"),
                list(vec![list(vec![atom("a"), atom("2")])]),
                list(vec![atom("-"), atom("a")]),
            ])
        ),
    }
}
