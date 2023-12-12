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
pub(crate) fn parse(code: &str) -> Result<Option<Expression>, String> {
    let code = code.trim_end();
    if code.is_empty() {
        Ok(None)
    } else {
        let (rest, expr) = parse_expression(code)?;
        if rest.is_empty() {
            Ok(Some(expr))
        } else {
            Err(format!("Redundant expression: {}", rest))
        }
    }
}

fn parse_expression(code: &str) -> Result<(&str, Expression), String> {
    let code = code.trim_start();

    if let Some(stripped) = code.strip_prefix('(') {
        parse_car(stripped)
    } else {
        parse_atom(code)
    }
}

fn parse_car(code: &str) -> Result<(&str, Expression), String> {
    let code = code.trim_start();

    if let Some(code) = code.strip_prefix(')') {
        Ok((code, nil()))
    } else {
        let (code, car) = parse_expression(code)?;
        let (code, cdr) = parse_cdr(code)?;

        Ok((code, cons(car, cdr)))
    }
}

fn parse_cdr(code: &str) -> Result<(&str, Expression), String> {
    let code = code.trim_start();

    if let Some(code) = code.strip_prefix('.') {
        let (code, expr) = parse_expression(code)?;

        let code = code.trim_start();
        if let Some(code) = code.strip_prefix(')') {
            Ok((code, expr))
        } else {
            Err(format!("Unclosed parencesis: {}", code))
        }
    } else {
        parse_car(code)
    }
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
    token.parse::<i64>().ok().map(int)
}

#[cfg(test)]
mod tests {
    use super::parse;
    use crate::expression::{bool, cons, int, list, nil, symbol};

    #[test]
    fn test_parse_empty() {
        assert_eq!(parse(""), Ok(None));
    }

    #[test]
    fn test_parse_bool() {
        let cases = vec![("#t", true), ("#f", false)];
        for (code, value) in cases {
            let expected = Ok(Some(bool(value)));
            assert_eq!(parse(code), expected);
        }
    }

    #[test]
    fn test_parse_int() {
        let cases = vec![
            ("0", 0),
            ("+0", 0),
            ("-0", 0),
            ("42", 42),
            ("+123", 123),
            ("-7", -7),
            ("0222", 222),
            ("+0816", 816),
            ("-0002", -2),
        ];
        for (code, value) in cases {
            let expected = Ok(Some(int(value)));
            assert_eq!(parse(code), expected);
        }
    }

    #[test]
    fn test_parse_symbol() {
        let cases = vec!["x", "foo", "+", "-", "3d", "x1"];
        for sym in cases {
            let expected = Ok(Some(symbol(sym)));
            assert_eq!(parse(sym), expected);
        }
    }

    #[test]
    fn test_parse_list() {
        let cases = vec![
            ("()", nil()),
            (" (  ) ", nil()),
            ("(x)", list(vec![symbol("x")])),
            (" ( x ) ", list(vec![symbol("x")])),
            ("(#t 2 y)", list(vec![bool(true), int(2), symbol("y")])),
            (
                "(let ((a 2)) (- a))",
                list(vec![
                    symbol("let"),
                    list(vec![list(vec![symbol("a"), int(2)])]),
                    list(vec![symbol("-"), symbol("a")]),
                ]),
            ),
        ];
        for (code, expr) in cases {
            let expected = Ok(Some(expr));
            assert_eq!(parse(code), expected);
        }
    }
}
