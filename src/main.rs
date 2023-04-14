use std::{
    collections::VecDeque,
    io::{self, BufRead, StdoutLock, Write},
};

const SYNTAX_TOKENS: [&str; 2] = ["(", ")"];

fn get_token(input: &str) -> (&str, &str) {
    let input = input.trim();

    let mut pos = 0;
    while pos < input.len() {
        let rest = &input[pos..];
        let is_end = rest.starts_with(|c: char| c.is_whitespace())
            || SYNTAX_TOKENS.iter().any(|&token| rest.starts_with(token));
        if is_end {
            break;
        }
        pos += 1;
    }

    input.split_at(pos.max(1))
}

fn tokenize(mut input: &str) -> Vec<&str> {
    let mut tokens = Vec::new();

    while input.len() > 0 {
        let (token, rest) = get_token(input);
        tokens.push(token);
        input = rest;
    }

    tokens
}

#[derive(Debug, Clone, PartialEq)]
enum SValue {
    Symbol(String),
    Number(f64),
    SpecialForm(String),
}

#[derive(Debug, Clone, PartialEq)]
enum SExpr {
    Atom(SValue),
    Nil,
    Cons(Box<SExpr>, Box<SExpr>),
}

impl SExpr {
    fn number(x: f64) -> SExpr {
        Self::Atom(SValue::Number(x))
    }

    fn symbol(s: String) -> SExpr {
        Self::Atom(SValue::Symbol(s))
    }

    fn cons(car: SExpr, cdr: SExpr) -> SExpr {
        Self::Cons(Box::new(car), Box::new(cdr))
    }
}

fn parse_list(tokens: &mut VecDeque<&str>) -> Result<SExpr, String> {
    match tokens.front() {
        Some(&")") => {
            tokens.pop_front();
            Ok(SExpr::Nil)
        }
        Some(_) => {
            let car = parse_s_expression(tokens)?;
            let cdr = parse_list(tokens)?;
            Ok(SExpr::cons(car, cdr))
        }
        None => Err("Unclosed open paren".to_string()),
    }
}

fn parse_s_expression(tokens: &mut VecDeque<&str>) -> Result<SExpr, String> {
    match tokens.pop_front() {
        Some("(") => parse_list(tokens),
        Some(token) => {
            let expr = match token.parse::<f64>() {
                Ok(x) => SExpr::number(x),
                Err(_) => SExpr::symbol(token.to_string()),
            };
            Ok(expr)
        }
        None => Err("Eof".to_string()),
    }
}

/// s_expression = atom | list
/// list         = "(" s_expression* ")"
/// atom         = number | symbol
fn parse(tokens: Vec<&str>) -> Result<SExpr, String> {
    let mut tokens = VecDeque::from(tokens);
    let expr = parse_s_expression(&mut tokens)?;

    if tokens.is_empty() {
        Ok(expr)
    } else {
        Err("Redundant expression".to_string())
    }
}

fn eval_value(value: SValue) -> Result<SValue, String> {
    match value {
        SValue::Symbol(s) => match s.as_str() {
            "+" | "-" | "*" | "/" => Ok(SValue::SpecialForm(s)),
            _ => Err(s),
        },
        SValue::Number(x) => Ok(SValue::Number(x)),
        SValue::SpecialForm(_) => Ok(value), // Handle special form
    }
}

fn args_to_numbers(args: Vec<SValue>) -> Result<Vec<f64>, SValue> {
    args.into_iter()
        .try_fold(Vec::new(), |mut acc, value| match value {
            SValue::Number(x) => {
                acc.push(x);
                Ok(acc)
            }
            SValue::Symbol(_) | SValue::SpecialForm(_) => Err(value),
        })
}

fn eval_special_form(symbol: &str, args: Vec<SValue>) -> Result<SValue, String> {
    match symbol {
        "+" => {
            let args =
                args_to_numbers(args).map_err(|value| format!("Invalid argument: {:?}", value))?;
            Ok(SValue::Number(args.iter().sum()))
        }
        "-" => {
            let args =
                args_to_numbers(args).map_err(|value| format!("Invalid argument: {:?}", value))?;
            match args.split_first() {
                Some((init, rest)) => {
                    let result = if rest.len() == 0 {
                        -init
                    } else {
                        rest.iter().fold(*init, |acc, x| acc - *x)
                    };
                    Ok(SValue::Number(result))
                }
                None => Err(format!("At least one argument required for: {}", symbol)),
            }
        }
        "*" => {
            let args =
                args_to_numbers(args).map_err(|value| format!("Invalid argument: {:?}", value))?;
            let result = args.iter().fold(1f64, |acc, x| acc * *x);
            Ok(SValue::Number(result))
        }
        "/" => {
            let args =
                args_to_numbers(args).map_err(|value| format!("Invalid argument: {:?}", value))?;
            match args.split_first() {
                Some((first, rest)) => {
                    let result = if rest.len() == 0 {
                        1f64 / *first
                    } else {
                        rest.iter().fold(*first, |acc, x| acc / *x)
                    };
                    Ok(SValue::Number(result))
                }
                None => Err(format!("At least one argument required for: {}", symbol)),
            }
        }
        _ => Err(format!("Invalid special form {}", symbol)),
    }
}

fn eval_args(mut expr: SExpr) -> Result<Vec<SValue>, String> {
    let mut args = Vec::new();
    loop {
        match expr {
            SExpr::Cons(car, cdr) => {
                let value = eval(*car)?;
                args.push(value);
                expr = *cdr;
            }
            SExpr::Nil => break,
            SExpr::Atom(value) => {
                return Err(format!(
                    "Arguments are not given as list. It ends by {:?}",
                    value
                ))
            }
        }
    }
    Ok(args)
}

fn eval_apply(car: SExpr, cdr: SExpr) -> Result<SValue, String> {
    match eval(car)? {
        SValue::Symbol(_) => todo!("Eval symbol"),
        SValue::SpecialForm(s) => {
            let args = eval_args(cdr)?;
            eval_special_form(s.as_str(), args)
        }
        SValue::Number(x) => Err(format!("Invalid application: {:?}", x)),
    }
}

fn eval(expr: SExpr) -> Result<SValue, String> {
    match expr {
        SExpr::Atom(value) => eval_value(value),
        SExpr::Nil => todo!("Evaluate Nil"),
        SExpr::Cons(car, cdr) => eval_apply(*car, *cdr),
    }
}

fn print_prompt(stdout: &mut StdoutLock) -> io::Result<()> {
    stdout.write("> ".as_bytes())?;
    stdout.flush()
}

fn main() {
    let stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();

    let mut lines = stdin.lines();
    loop {
        print_prompt(&mut stdout).unwrap();

        if let Some(Ok(line)) = lines.next() {
            let tokens = tokenize(&line);
            match parse(tokens) {
                Ok(expr) => match eval(expr) {
                    Ok(value) => println!("{:?}", value),
                    Err(error) => println!("Eval error: {}", error),
                },
                Err(error) => {
                    println!("Parse error: {}", error);
                }
            }
        } else {
            println!("\nBye");
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenize;

    #[test]
    fn test_tokenize() {
        let input = r#"
        (define (fact n)
            (if (< n 2) 1 (* n (fact (- n 1))))) "#;
        let actual = tokenize(input);
        assert_eq!(
            actual,
            vec![
                "(", "define", "(", "fact", "n", ")", "(", "if", "(", "<", "n", "2", ")", "1", "(",
                "*", "n", "(", "fact", "(", "-", "n", "1", ")", ")", ")", ")", ")"
            ]
        );
    }
}
