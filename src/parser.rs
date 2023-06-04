use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{char, digit1, multispace0, one_of};
use nom::combinator::{all_consuming, map_res, opt};

use nom::error::ParseError;
use nom::sequence::{delimited, terminated};
use nom::{AsChar, Parser};
use nom::{IResult, InputTakeAtPosition};

use crate::expression::SExpression;

fn trim<I, O, E: ParseError<I>, F>(f: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    F: Parser<I, O, E>,
{
    terminated(f, multispace0)
}

fn parse_bool(code: &str) -> IResult<&str, SExpression> {
    let (code, tag) = alt((tag("#t"), tag("#f")))(code)?;

    Ok((code, SExpression::bool(tag == "#t")))
}

fn parse_int(code: &str) -> IResult<&str, SExpression> {
    let (code, sign) = opt(one_of("+-"))(code)?;
    let sign = sign.unwrap_or('+');

    let (code, n) = map_res(digit1, |n: &str| n.parse::<i64>())(code)?;
    let n = if sign == '+' { n } else { -n };

    Ok((code, SExpression::int(n)))
}

fn parse_token(code: &str) -> IResult<&str, &str> {
    const DELIMITERS: &str = r#"(){}[];"'`|"#;
    take_while1(|c: char| !c.is_whitespace() && !DELIMITERS.contains(c))(code)
}

fn parse_atom(code: &str) -> IResult<&str, SExpression> {
    let (code, token) = trim(parse_token)(code)?;

    match all_consuming(alt((parse_bool, parse_int)))(token) {
        Ok((_, expr)) => Ok((code, expr)),
        Err(_) => Ok((code, SExpression::symbol(token))),
    }
}

fn parse_cdr(code: &str) -> IResult<&str, SExpression> {
    let (code, dot) = opt(trim(char('.')))(code)?;

    match dot {
        Some(_) => {
            let (code, expr) = trim(parse_expression)(code)?;
            let (code, _) = trim(char(')'))(code)?;

            Ok((code, expr))
        }
        None => parse_car(code),
    }
}

fn parse_car(code: &str) -> IResult<&str, SExpression> {
    let (code, rparen) = opt(trim(char(')')))(code)?;
    match rparen {
        Some(_) => Ok((code, SExpression::nil())),
        None => {
            let (code, car) = trim(parse_expression)(code)?;
            let (code, cdr) = trim(parse_cdr)(code)?;

            Ok((code, SExpression::cons(car, cdr)))
        }
    }
}

fn parse_expression(code: &str) -> IResult<&str, SExpression> {
    let (code, lparen) = opt(trim(char('(')))(code)?;
    match lparen {
        Some(_) => trim(parse_car)(code),
        None => trim(parse_atom)(code),
    }
}

pub fn parse(code: &str) -> IResult<&str, SExpression> {
    delimited(multispace0, parse_expression, multispace0)(code)
}

#[cfg(test)]
mod tests {
    use crate::{expression::SExpression, parser::parse};

    #[test]
    fn parse_bool_test() {
        assert_eq!(Ok(("", SExpression::bool(true))), parse("#t"));
        assert_eq!(Ok(("", SExpression::bool(false))), parse("#f"));
    }

    #[test]
    fn parse_int_test() {
        assert_eq!(Ok(("", SExpression::int(0))), parse("0"));
        assert_eq!(Ok(("", SExpression::int(1))), parse("+1"));
        assert_eq!(Ok(("", SExpression::int(-2))), parse("-2"));
        assert_eq!(Ok(("", SExpression::int(345))), parse("345"));
        assert_eq!(Ok(("", SExpression::int(-678))), parse("-00678"));
    }

    #[test]
    fn parse_symbol_test() {
        assert_eq!(Ok(("", SExpression::symbol("x"))), parse("x"));
        assert_eq!(Ok(("", SExpression::symbol("foo"))), parse("foo"));
        assert_eq!(Ok(("", SExpression::symbol("x1"))), parse("x1"));
        assert_eq!(Ok(("", SExpression::symbol("3d"))), parse("3d"));
        assert_eq!(Ok(("", SExpression::symbol("+"))), parse("+"));
        assert_eq!(Ok(("", SExpression::symbol("-"))), parse("-"));
        assert_eq!(Ok((")", SExpression::symbol("x"))), parse("x)"));
        assert_eq!(Ok(("bar", SExpression::symbol("foo"))), parse("foo bar"));
    }

    #[test]
    fn parse_list() {
        assert_eq!(Ok(("", SExpression::nil())), parse("()"));
        assert_eq!(Ok(("", SExpression::nil())), parse(" ( ) "));

        assert_eq!(
            Ok(("", SExpression::list(vec![SExpression::symbol("x")]))),
            parse("(x)")
        );
        assert_eq!(
            Ok(("", SExpression::list(vec![SExpression::symbol("x")]))),
            parse(" ( x ) ")
        );

        let expr = SExpression::list(vec![
            SExpression::bool(true),
            SExpression::int(2),
            SExpression::symbol("y"),
        ]);
        assert_eq!(Ok(("", expr)), parse("(#t 2 y)"));

        let expr = SExpression::cons(
            SExpression::symbol("x"),
            SExpression::cons(SExpression::symbol("y"), SExpression::symbol("z")),
        );
        assert_eq!(Ok(("", expr)), parse("(x y . z)"));

        let expr = SExpression::list(vec![
            SExpression::symbol("x"),
            SExpression::symbol("y."),
            SExpression::symbol("z"),
        ]);
        assert_eq!(Ok(("", expr)), parse("(x y. z)"));

        let expr = SExpression::list(vec![SExpression::symbol("x"), SExpression::symbol("y")]);
        assert_eq!(Ok(("", expr)), parse("(x y . ())"));

        let expr = SExpression::list(vec![
            SExpression::symbol("let"),
            SExpression::list(vec![SExpression::list(vec![
                SExpression::symbol("a"),
                SExpression::int(2),
            ])]),
            SExpression::list(vec![SExpression::symbol("-"), SExpression::symbol("a")]),
        ]);
        assert_eq!(Ok(("", expr)), parse("(let ((a 2)) (- a))"));
    }
}
