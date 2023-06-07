use std::collections::VecDeque;

use crate::value::Value;

fn expect_numbers(values: Vec<Value>) -> Result<Vec<i64>, String> {
    values
        .into_iter()
        .try_fold(Vec::new(), |mut numbers, value| match value {
            Value::Int(n) => {
                numbers.push(n);
                Ok(numbers)
            }
            _ => Err(format!("Type Error: Number expected: {}", value)),
        })
}

fn built_in_arithmetic_operation<F: Fn(i64, i64) -> i64>(
    name: &str,
    args: Vec<Value>,
    f: F,
    unit: i64,
    commutative: bool,
) -> Result<Value, String> {
    let mut numbers = VecDeque::from_iter(expect_numbers(args)?);
    if commutative {
        Ok(Value::Int(numbers.into_iter().fold(unit, f)))
    } else if let Some(first) = numbers.pop_front() {
        if numbers.len() == 0 {
            Ok(Value::Int(f(unit, first)))
        } else {
            Ok(Value::Int(numbers.into_iter().fold(first, f)))
        }
    } else {
        Err(format!(
            "Eval Error: Procedure reuqires at least one artument: {}",
            name
        ))
    }
}

pub fn built_in_add() -> Value {
    Value::built_in_proc("+", |args: Vec<Value>| {
        built_in_arithmetic_operation("+", args, |x, y| x + y, 0, true)
    })
}

pub fn built_in_sub() -> Value {
    Value::built_in_proc("-", |args: Vec<Value>| {
        built_in_arithmetic_operation("-", args, |x, y| x - y, 0, false)
    })
}

pub fn built_in_mul() -> Value {
    Value::built_in_proc("*", |args: Vec<Value>| {
        built_in_arithmetic_operation("*", args, |x, y| x * y, 1, true)
    })
}

pub fn built_in_div() -> Value {
    Value::built_in_proc("*", |args: Vec<Value>| {
        built_in_arithmetic_operation("/", args, |x, y| x / y, 1, false)
    })
}

#[cfg(test)]
mod tests {
    use crate::{eval::eval, parser::parse, value::Value};

    #[test]
    fn eval_arithmetic_operators_test() {
        let cases = vec![
            ("(+)", 0),
            ("(+ 10)", 10),
            ("(+ 1 2)", 3),
            ("(+ 1 2 3 4)", 10),
            ("(- 10)", -10),
            ("(- 1 2)", -1),
            ("(- 1000 100 10 1)", 889),
            ("(*))", 1),
            ("(* 5))", 5),
            ("(* 2 3))", 6),
            ("(* 1 2 3 4))", 24),
            ("(/ 2))", 0),
            ("(/ 15 4))", 3),
            ("(/ 1000 5 2 10))", 10),
        ];
        cases.into_iter().for_each(|(input, expected)| {
            let (_, expr) = parse(input).unwrap();
            let actual = eval(expr).unwrap();
            assert_eq!(actual, Value::int(expected));
        });
    }
}
