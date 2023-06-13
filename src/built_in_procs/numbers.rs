use std::collections::VecDeque;

use crate::{
    env::Env,
    expression::{built_in_proc, int, Expression, ExpressionData},
    value::Value,
};

fn expect_numbers(name: &str, values: Vec<Expression>) -> Result<Vec<i64>, String> {
    values
        .into_iter()
        .try_fold(Vec::new(), |mut numbers, expr| match *expr {
            ExpressionData::Atom(Value::Int(number)) => {
                numbers.push(number);
                Ok(numbers)
            }
            _ => Err(format!("Type Error: number expected: {}", name)),
        })
}

fn calc_arithmetic_operation<F: Fn(i64, i64) -> i64>(
    name: &str,
    args: Vec<Expression>,
    op: F,
    unit: i64,
    commutative: bool,
) -> Result<Expression, String> {
    let numbers = expect_numbers(name, args)?;
    if commutative {
        Ok(int(numbers.into_iter().fold(unit, op)))
    } else {
        let mut numbers = VecDeque::from_iter(numbers);
        if let Some(first) = numbers.pop_front() {
            if numbers.len() == 0 {
                Ok(int(op(unit, first)))
            } else {
                Ok(int(numbers.into_iter().fold(first, op)))
            }
        } else {
            Err(format!(
                "Eval Error: Procedure reuqires at least one artument: {}",
                name
            ))
        }
    }
}

fn proc_add(args: Vec<Expression>) -> Result<Expression, String> {
    calc_arithmetic_operation("+", args, |x, y| x + y, 0, true)
}

fn proc_sub(args: Vec<Expression>) -> Result<Expression, String> {
    calc_arithmetic_operation("-", args, |x, y| x - y, 0, false)
}

fn proc_mul(args: Vec<Expression>) -> Result<Expression, String> {
    calc_arithmetic_operation("*", args, |x, y| x * y, 1, true)
}

fn proc_div(args: Vec<Expression>) -> Result<Expression, String> {
    calc_arithmetic_operation("/", args, |x, y| x / y, 1, false)
}

pub fn define_procs(env: &Env) {
    env.set("+", built_in_proc("+", proc_add));
    env.set("-", built_in_proc("-", proc_sub));
    env.set("*", built_in_proc("*", proc_mul));
    env.set("/", built_in_proc("/", proc_div));
}

#[cfg(test)]
mod tests {
    use crate::expression::{bool, int, Expression};

    use super::{proc_add, proc_div, proc_mul, proc_sub};

    #[test]
    fn eval_arithmetic_operators_test() {
        type Case = (
            fn(Vec<Expression>) -> Result<Expression, String>,
            Vec<i64>,
            i64,
        );
        let cases: Vec<Case> = vec![
            (proc_add, vec![], 0),
            (proc_add, vec![10], 10),
            (proc_add, vec![1, 2], 3),
            (proc_add, vec![1, 2, 3, 4], 10),
            (proc_sub, vec![10], -10),
            (proc_sub, vec![1, 2], -1),
            (proc_sub, vec![1000, 100, 10, 1], 889),
            (proc_mul, vec![], 1),
            (proc_mul, vec![5], 5),
            (proc_mul, vec![2, 3], 6),
            (proc_mul, vec![1, 2, 3, 4], 24),
            (proc_div, vec![2], 0),
            (proc_div, vec![15, 4], 3),
            (proc_div, vec![1000, 5, 2, 10], 10),
        ];
        cases.into_iter().for_each(|(proc, operands, expected)| {
            let values = operands.into_iter().map(|n| int(n)).collect();
            let actual = proc(values).unwrap();
            assert_eq!(actual, int(expected));
        });
    }

    #[test]
    fn eval_arithmetic_operators_error_test() {
        type Case = (
            fn(Vec<Expression>) -> Result<Expression, String>,
            Vec<Expression>,
        );
        let cases: Vec<Case> = vec![
            (proc_add, vec![int(2), bool(false)]),
            (proc_sub, vec![]),
            (proc_div, vec![]),
        ];
        cases.into_iter().for_each(|(proc, args)| {
            assert!(proc(args).is_err());
        });
    }
}
