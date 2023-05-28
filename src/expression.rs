use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Bool(bool),
    Int(i64),
    Symbol(String),
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Bool(b) if *b => write!(f, "#t"),
            Primitive::Bool(_) => write!(f, "#f"),
            Primitive::Int(n) => write!(f, "{}", n),
            Primitive::Symbol(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SExpression {
    Nil,
    Atom(Primitive),
    Cons(Box<SExpression>, Box<SExpression>),
}

impl SExpression {
    pub fn nil() -> Self {
        Self::Nil
    }

    pub fn bool(b: bool) -> Self {
        Self::Atom(Primitive::Bool(b))
    }

    pub fn int(n: i64) -> Self {
        Self::Atom(Primitive::Int(n))
    }

    pub fn symbol<S: Into<String>>(s: S) -> Self {
        Self::Atom(Primitive::Symbol(s.into()))
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
    use super::SExpression;

    fn simple_list_elems() -> Vec<SExpression> {
        vec![SExpression::int(1), SExpression::int(2)]
    }

    fn simple_list() -> SExpression {
        SExpression::list(simple_list_elems())
    }

    fn incomplete_list() -> SExpression {
        SExpression::cons(
            SExpression::int(1),
            SExpression::cons(SExpression::int(2), SExpression::int(3)),
        )
    }

    fn let_expression_elems() -> Vec<SExpression> {
        vec![
            SExpression::symbol("let"),
            SExpression::list(vec![SExpression::list(vec![
                SExpression::symbol("a"),
                SExpression::int(2),
            ])]),
            SExpression::list(vec![SExpression::symbol("-"), SExpression::symbol("a")]),
        ]
    }

    fn let_expression() -> SExpression {
        SExpression::list(let_expression_elems())
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
        flatten_nil: (Some(vec![]), SExpression::nil()),
        flatten_atom: (None, SExpression::symbol("x")),
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
        display_nil: ("()", SExpression::nil()),
        display_atom: ("x", SExpression::symbol("x")),
        display_cons: ("(() . 0)", SExpression::cons(SExpression::nil(), SExpression::int(0))),
        display_simple_list: ("(1 2)", simple_list()),
        display_incomplete_list: ("(1 2 . 3)", incomplete_list()),
        display_let_expression: ("(let ((a 2)) (- a))", let_expression()),
    }
}
