use crate::expression::{bool, cons, int, nil, symbol, Expression};

/// ```
/// EXPRESSION ::= "(" CAR | ATOM
/// CAR        ::= ")" | S_EXPRESSION CDR
/// CDR        ::= "." S_EXPRESSION ")" | CAR
/// ATOM       ::= BOOL | INT | SYMBOL
/// BOOL       ::= "#t" | "#f"
/// INT        ::= [+-]? [0-9]+
/// SYMBOL     ::= [^(){}\[\];"'`|]
/// ```
pub(crate) fn parse(code: &str) -> Result<Expression, String> {
    let (code, expr) = parse_expression(code)?;

    let code = code.trim_end();
    if code.is_empty() {
        Ok(expr)
    } else {
        Err(format!("Redundant expression: {}", code))
    }
}

fn parse_expression(code: &str) -> Result<(&str, Expression), String> {
    let code = code.trim_start();
    parse_atom(code)
}

fn parse_car(code: &str) -> Result<(&str, Expression), String> {
    todo!()
}

fn parse_cdr(code: &str) -> Result<(&str, Expression), String> {
    todo!()
}

fn parse_atom(code: &str) -> Result<(&str, Expression), String> {
    let code = code.trim_start();
    match parse_token(code) {
        Some((rest, token)) => {
            if let Some(expr) = parse_bool(token) {
                Ok((rest, expr))
            } else if let Some(expr) = parse_int(token) {
                Ok((rest, expr))
            } else {
                Ok((rest, symbol(token.to_string())))
            }
        }
        None => Err("Unexpected EOF".to_string()),
    }
}

fn parse_token(code: &str) -> Option<(&str, &str)> {
    const DELIMITERS: &str = r#"(){}[];"'`|"#;
    let end = code
        .find(|c: char| c.is_whitespace() || DELIMITERS.contains(c))
        .unwrap_or(code.len());

    if end == 0 {
        None
    } else {
        let (token, rest) = code.split_at(end);
        Some((rest, token))
    }
}

fn parse_bool(token: &str) -> Option<Expression> {
    match token {
        "#t" | "#f" => Some(bool(token == "#t")),
        _ => None,
    }
}

fn parse_int(token: &str) -> Option<Expression> {
    token.parse::<i64>().ok().map(|num| int(num))
}

#[cfg(test)]
mod tests {
    use super::parse;
    use crate::expression::{bool, cons, int, list, nil, symbol};

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse("#t"), Ok((bool(true))));
        assert_eq!(parse("#f"), Ok((bool(false))));
    }

    #[test]
    fn test_parse_int() {
        assert_eq!(parse("0"), Ok(int(0)));
        assert_eq!(parse("+0"), Ok(int(0)));
        assert_eq!(parse("-0"), Ok(int(0)));
        assert_eq!(parse("42"), Ok(int(42)));
        assert_eq!(parse("+123"), Ok(int(123)));
        assert_eq!(parse("-7"), Ok(int(-7)));
        assert_eq!(parse("0222"), Ok(int(222)));
        assert_eq!(parse("+00816"), Ok(int(816)));
        assert_eq!(parse("-0002"), Ok(int(-2)));
    }

    #[test]
    fn test_parse_symbol() {
        assert_eq!(parse("x"), Ok(symbol("x")));
        assert_eq!(parse("foo"), Ok(symbol("foo")));
        assert_eq!(parse("+"), Ok(symbol("+")));
        assert_eq!(parse("-"), Ok(symbol("-")));
    }
}
