use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::{digit1, one_of};
use nom::combinator::{all_consuming, map_res, opt};
use nom::IResult;

use crate::expression::SExpression;

fn parse_bool(s: &str) -> IResult<&str, SExpression> {
    let (s, tag) = alt((tag("#t"), tag("#f")))(s)?;
    let b = tag == "#t";

    Ok((s, SExpression::bool(b)))
}

fn parse_int(s: &str) -> IResult<&str, SExpression> {
    let (s, sign) = opt(one_of("+-"))(s)?;
    let sign = sign.unwrap_or('+');

    let (s, n) = map_res(digit1, |n: &str| n.parse::<i64>())(s)?;
    let n = if sign == '+' { n } else { -n };

    Ok((s, SExpression::int(n)))
}

fn parse_token(s: &str) -> IResult<&str, &str> {
    const DELIMITERS: &str = r#"(){}[];"'`|"#;
    take_while1(|c: char| !c.is_whitespace() && !DELIMITERS.contains(c))(s)
}

fn parse_atom(s: &str) -> IResult<&str, SExpression> {
    let (s, token) = parse_token(s)?;
    match all_consuming(alt((parse_bool, parse_int)))(token) {
        Ok((_, expr)) => Ok((s, expr)),
        Err(_) => Ok((s, SExpression::symbol(token))),
    }
}

pub fn parse(input: &str) -> IResult<&str, SExpression> {
    parse_atom(input)
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
        assert_eq!(Ok((" bar", SExpression::symbol("foo"))), parse("foo bar"));
    }
}
