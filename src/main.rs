use std::io::{self, BufRead, Write, StdoutLock};

const ONE_LETTER_TOKENS: &str = "()";

fn tokenize(input: &str) -> Vec<String> {
    let mut tokens = Vec::new();

    let mut input = input.trim();
    while !input.is_empty() {
        let chr = input.chars().nth(0).unwrap();
        if chr.is_whitespace() {
            input = input.trim_start();
        } else if ONE_LETTER_TOKENS.contains(chr) {
            tokens.push(chr.to_string());
            input = &input[1..];
        } else {
            let end = input
                .find(|c: char| c.is_whitespace() || ONE_LETTER_TOKENS.contains(c))
                .unwrap_or(input.len());
            let (token, rest) = input.split_at(end);
            tokens.push(token.to_string());
            input = rest;
        }
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

/// s_expression = atom | list
/// list         = "(" list_loop ")"
/// list_loop    = "" | s_expression list_loop
/// atom         = number | symbol
fn parse(input: &str) -> Result<SExpr, String> {
    todo!()
}

fn print_prompt(stdout: &mut StdoutLock) -> io::Result<()>{
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
            (if (< n 2) 1 (* n (fact (- n 1)))))"#;
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
