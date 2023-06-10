mod expression;
mod parser;
mod value;
mod eval;
mod built_in_procs;
mod env;

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
enum SExpr<T> {
    Nil,
    Atom(T),
    Cons(Box<SExpr<T>>, Box<SExpr<T>>),
}

impl<T> SExpr<T> {
    fn cons(car: Self, cdr: Self) -> Self {
        Self::Cons(Box::new(car), Box::new(cdr))
    }
}

type Skelton = SExpr<String>;

fn parse_list(tokens: &mut VecDeque<&str>) -> Result<Skelton, String> {
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

fn parse_s_expression(tokens: &mut VecDeque<&str>) -> Result<Skelton, String> {
    match tokens.pop_front() {
        Some("(") => parse_list(tokens),
        Some(")") => Err("Unexpected token: )".to_string()),
        Some(token) => Ok(SExpr::Atom(token.to_string())),
        None => Err("Unexpected EOF".to_string()),
    }
}

/// s_expression = atom | list
/// list         = "(" s_expression* ")"
/// atom         = number | symbol
fn parse(tokens: Vec<&str>) -> Result<Skelton, String> {
    let mut tokens = VecDeque::from(tokens);
    let expr = parse_s_expression(&mut tokens)?;

    if tokens.is_empty() {
        Ok(expr)
    } else {
        Err("Redundant expression".to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
enum SValue {
    Nil,
    Symbol(String),
    Number(f64),
    BuiltInFunc(String),
}

fn eval_atom(token: String) -> Result<SValue, String> {
    if let Ok(x) = token.parse::<f64>() {
        Ok(SValue::Number(x))
    } else {
        match token.as_str() {
            "+" | "-" | "*" | "/" => Ok(SValue::BuiltInFunc(token)),
            _ => Ok(SValue::Symbol(token)),
        }
    }
}

fn eval_args(mut expr: Skelton) -> Result<Vec<SValue>, String> {
    let mut args = Vec::new();
    loop {
        match expr {
            SExpr::Nil => break,
            SExpr::Atom(value) => {
                return Err(format!(
                    "Arguments are not given as list. It ends by {:?}",
                    value
                ))
            }
            SExpr::Cons(car, cdr) => {
                let value = eval_expr(*car)?;
                args.push(value);
                expr = *cdr;
            }
        }
    }
    Ok(args)
}

fn try_map_to_numbers(args: Vec<SValue>) -> Result<Vec<f64>, SValue> {
    args.into_iter()
        .try_fold(Vec::new(), |mut acc, value| match value {
            SValue::Number(x) => {
                acc.push(x);
                Ok(acc)
            }
            _ => Err(value),
        })
}

fn eval_builtin_func(name: String, expr: Skelton) -> Result<SValue, String> {
    let args = eval_args(expr)?;

    match name.as_str() {
        "+" => {
            let args =
                try_map_to_numbers(args).map_err(|value| format!("Invalid argument: {value:?}"))?;
            Ok(SValue::Number(args.iter().sum()))
        }
        "-" => {
            let args =
                try_map_to_numbers(args).map_err(|value| format!("Invalid argument: {value:?}"))?;
            match args.split_first() {
                Some((init, rest)) => {
                    let result = if rest.len() == 0 {
                        -init
                    } else {
                        rest.iter().fold(*init, |acc, x| acc - *x)
                    };
                    Ok(SValue::Number(result))
                }
                None => Err(format!("At least one argument required for: {name}")),
            }
        }
        "*" => {
            let args =
                try_map_to_numbers(args).map_err(|value| format!("Invalid argument: {value:?}"))?;
            let result = args.iter().fold(1f64, |acc, x| acc * *x);
            Ok(SValue::Number(result))
        }
        "/" => {
            let args =
                try_map_to_numbers(args).map_err(|value| format!("Invalid argument: {value:?}"))?;
            match args.split_first() {
                Some((first, rest)) => {
                    let result = if rest.len() == 0 {
                        1f64 / *first
                    } else {
                        rest.iter().fold(*first, |acc, x| acc / *x)
                    };
                    Ok(SValue::Number(result))
                }
                None => Err(format!("At least one argument required for: {name}")),
            }
        }
        _ => unreachable!("Invalid built-in function: {name}"),
    }
}

fn eval_apply(car: Skelton, cdr: Skelton) -> Result<SValue, String> {
    match eval_expr(car)? {
        SValue::BuiltInFunc(name) => eval_builtin_func(name, cdr),
        value => Err(format!("Invalid application: {value:?}")),
    }
}

fn eval_expr(expr: Skelton) -> Result<SValue, String> {
    match expr {
        SExpr::Nil => Ok(SValue::Nil),
        SExpr::Atom(value) => eval_atom(value),
        SExpr::Cons(car, cdr) => eval_apply(*car, *cdr),
    }
}

fn eval(expr: Skelton) -> Result<SValue, String> {
    // TODO: build initial environment.
    eval_expr(expr)
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
                    Ok(value) => println!("{value:?}"),
                    Err(error) => println!("Eval error: {error}"),
                },
                Err(error) => {
                    println!("Parse error: {error}");
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
