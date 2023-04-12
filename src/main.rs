use std::{
    collections::VecDeque,
    io::{self, BufRead, StdoutLock, Write},
};

const SPECIAL_TOKENS: [&str; 2] = ["(", ")"];

fn get_token(input: &str) -> (&str, &str) {
    let input = input.trim();

    let mut pos = 0;
    while pos < input.len() {
        let rest = &input[pos..];
        let is_end = rest.starts_with(|c: char| c.is_whitespace())
            || SPECIAL_TOKENS.iter().any(|&token| rest.starts_with(token));
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
enum Value {
    Symbol(String),
    Number(f64),
}

#[derive(Debug, Clone, PartialEq)]
enum SExpr {
    Atom(Value),
    Nil,
    Cons(Box<SExpr>, Box<SExpr>),
}

fn parse_s_expression(tokens: VecDeque<String>) -> Result<SExpr, String> {
    todo!()
}

/// s_expression = atom | list
/// list         = "(" list_loop ")"
/// list_loop    = "" | s_expression list_loop
/// atom         = number | symbol
fn parse(tokens: Vec<String>) -> Result<SExpr, String> {
    todo!()
}

fn print_prompt(stdout: &mut StdoutLock) -> io::Result<()> {
    stdout.write("> ".as_bytes())?;
    stdout.flush()
}

fn main() {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    let mut stdout = io::stdout().lock();

    loop {
        print_prompt(&mut stdout).unwrap();

        if let Some(Ok(line)) = lines.next() {
            let tokens = tokenize(&line);
            println!("Tokenized: {:?}", tokens);
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
