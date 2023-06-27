use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{char, digit1, multispace0, one_of};
use nom::combinator::{all_consuming, map_res, opt};

use nom::error::VerboseError;
use nom::sequence::{preceded, terminated};
use nom::IResult;

use crate::expression::{bool, cons, int, nil, symbol, Expression};

macro_rules! trim {
    ($f: expr) => {
        preceded(multispace0, $f)
    };
}

fn parse_bool(code: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (code, tag) = alt((tag("#t"), tag("#f")))(code)?;

    Ok((code, bool(tag == "#t")))
}

fn parse_int(code: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (code, sign) = opt(one_of("+-"))(code)?;
    let sign = sign.unwrap_or('+');

    let (code, n) = map_res(digit1, |n: &str| n.parse::<i64>())(code)?;
    let n = if sign == '+' { n } else { -n };

    Ok((code, int(n)))
}

fn parse_token(code: &str) -> IResult<&str, &str, VerboseError<&str>> {
    const DELIMITERS: &str = r#"(){}[];"'`|"#;
    take_while1(|c: char| !c.is_whitespace() && !DELIMITERS.contains(c))(code)
}

fn parse_atom(code: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    let (code, token) = trim!(parse_token)(code)?;

    match all_consuming(alt((parse_bool, parse_int)))(token) {
        Ok((_, expr)) => Ok((code, expr)),
        Err(_) => Ok((code, symbol(token))),
    }
}

fn parse_cdr(code: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    match opt(trim!(char('.')))(code)? {
        (code, None) => parse_car(code),
        (code, _) => terminated(parse_expression, trim!(char(')')))(code),
    }
}

fn parse_car(code: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    match opt(trim!(char(')')))(code)? {
        (code, None) => {
            let (code, car) = parse_expression(code)?;
            let (code, cdr) = parse_cdr(code)?;

            Ok((code, cons(car, cdr)))
        }
        (code, _) => Ok((code, nil())),
    }
}

fn parse_expression(code: &str) -> IResult<&str, Expression, VerboseError<&str>> {
    match opt(trim!(char('(')))(code)? {
        (code, None) => parse_atom(code),
        (code, _) => parse_car(code),
    }
}

/// ```
/// EXPRESSION ::= "(" CAR | ATOM
/// CAR        ::= ")" | S_EXPRESSION CDR
/// CDR        ::= "." S_EXPRESSION ")" | CAR
/// ATOM       ::= BOOL | INT | SYMBOL
/// BOOL       ::= "#t" | "#f"
/// INT        ::= [+-]? [0-9]+
/// SYMBOL     ::= [^(){}\[\];"'`|]
/// ```
pub(crate) fn parse(code: &str) -> IResult<&str, Option<Expression>, VerboseError<&str>> {
    let (code, _) = multispace0(code)?;
    if code.is_empty() {
        Ok((code, None))
    } else {
        let (rest, expr) = terminated(parse_expression, multispace0)(code)?;
        Ok((rest, Some(expr)))
    }
}

#[cfg(test)]
mod tests {
    use super::parse;
    use crate::expression::{bool, cons, int, list, nil, symbol};

    #[test]
    fn parse_empty_test() {
        assert_eq!(Ok(("", None)), parse(""));
        assert_eq!(Ok(("", None)), parse("  "));
        assert_eq!(Ok(("", None)), parse("\n\t"));
    }

    #[test]
    fn parse_bool_test() {
        assert_eq!(Ok(("", Some(bool(true)))), parse("#t"));
        assert_eq!(Ok(("", Some(bool(false)))), parse("#f"));
    }

    #[test]
    fn parse_int_test() {
        assert_eq!(Ok(("", Some(int(0)))), parse("0"));
        assert_eq!(Ok(("", Some(int(1)))), parse("+1"));
        assert_eq!(Ok(("", Some(int(-2)))), parse("-2"));
        assert_eq!(Ok(("", Some(int(345)))), parse("345"));
        assert_eq!(Ok(("", Some(int(-678)))), parse("-00678"));
    }

    #[test]
    fn parse_symbol_test() {
        assert_eq!(Ok(("", Some(symbol("x")))), parse("x"));
        assert_eq!(Ok(("", Some(symbol("foo")))), parse("foo"));
        assert_eq!(Ok(("", Some(symbol("x1")))), parse("x1"));
        assert_eq!(Ok(("", Some(symbol("3d")))), parse("3d"));
        assert_eq!(Ok(("", Some(symbol("+")))), parse("+"));
        assert_eq!(Ok(("", Some(symbol("-")))), parse("-"));
        assert_eq!(Ok((")", Some(symbol("x")))), parse("x)"));
        assert_eq!(Ok(("bar", Some(symbol("foo")))), parse("foo bar"));
    }

    #[test]
    fn parse_list_test() {
        assert_eq!(Ok(("", Some(nil()))), parse("()"));
        assert_eq!(Ok(("", Some(nil()))), parse(" ( ) "));

        assert_eq!(Ok(("", Some(list(vec![symbol("x")])))), parse("(x)"));
        assert_eq!(Ok(("", Some(list(vec![symbol("x")])))), parse(" ( x ) "));

        let expr = list(vec![bool(true), int(2), symbol("y")]);
        assert_eq!(Ok(("", Some(expr))), parse("(#t 2 y)"));

        let expr = list(vec![
            symbol("let"),
            list(vec![list(vec![symbol("a"), int(2)])]),
            list(vec![symbol("-"), symbol("a")]),
        ]);
        assert_eq!(Ok(("", Some(expr))), parse("(let ((a 2)) (- a))"));
    }

    #[test]
    fn parse_cons_test() {
        let expr = cons(symbol("x"), cons(symbol("y"), symbol("z")));
        assert_eq!(Ok(("", Some(expr))), parse("(x y . z)"));

        let expr = cons(list(vec![int(1), int(2)]), list(vec![int(3), int(4)]));
        assert_eq!(Ok(("", Some(expr))), parse("((1 2).(3 4))"));

        let expr = list(vec![symbol("x"), symbol("y."), symbol("z")]);
        assert_eq!(Ok(("", Some(expr))), parse("(x y. z)"));

        let expr = list(vec![symbol("x"), symbol("y")]);
        assert_eq!(Ok(("", Some(expr))), parse("(x y . ())"));
    }

    #[test]
    fn parse_error_test() {
        assert!(parse("").is_err());
        assert!(parse(")").is_err());
        assert!(parse("(1 2 3").is_err());
        assert!(parse("(1 . 2 3)").is_err());
        assert!(parse("(()").is_err());
    }
}
