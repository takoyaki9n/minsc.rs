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
            tokens.push(input[..end].to_string());
            input = &input[end..];
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

fn main() {
    println!("Hello World!");
}

#[cfg(test)]
mod tests {
    use crate::tokenize;

    #[test]
    fn test_tokenize() {
        let input = "(define (fact n)
            (if (< n 2) 1 (* n (fact (- n 1)))))";
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
