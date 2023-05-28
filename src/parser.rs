use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{digit1, one_of};
use nom::combinator::{map_res, opt};
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

fn parse_atom(s: &str) -> IResult<&str, SExpression> {
    alt((parse_bool, parse_int))(s)
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
}
