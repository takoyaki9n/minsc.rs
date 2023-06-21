use std::collections::VecDeque;

use crate::{
    env::Env,
    expression::{bool, built_in_proc, int, Expression, ExpressionInner},
    value::Value,
};

fn expect_numbers(name: &str, values: Vec<Expression>) -> Result<Vec<i64>, String> {
    values
        .into_iter()
        .try_fold(Vec::new(), |mut numbers, expr| match *expr {
            ExpressionInner::Atom(Value::Int(number)) => {
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
        let mut numbers = VecDeque::from(numbers);
        if let Some(first) = numbers.pop_front() {
            if numbers.is_empty() {
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

fn calc_arithmetic_comparison(
    name: &str,
    args: Vec<Expression>,
    cmp: impl Fn(i64, i64) -> bool,
) -> Result<Expression, String> {
    if args.len() < 2 {
        return Err(format!(
            "Wrong number of arguments for {}: required 2, got {}",
            name,
            args.len()
        ));
    }

    let mut numbers = VecDeque::from(expect_numbers(name, args)?);
    let mut prev = numbers.pop_front().unwrap();
    for number in numbers {
        if !cmp(prev, number) {
            return Ok(bool(false));
        }
        prev = number;
    }
    Ok(bool(true))
}

fn proc_eq(args: Vec<Expression>) -> Result<Expression, String> {
    calc_arithmetic_comparison("=", args, |x, y| x == y)
}

fn proc_lt(args: Vec<Expression>) -> Result<Expression, String> {
    calc_arithmetic_comparison("<", args, |x, y| x < y)
}

fn proc_le(args: Vec<Expression>) -> Result<Expression, String> {
    calc_arithmetic_comparison("<=", args, |x, y| x <= y)
}

fn proc_gt(args: Vec<Expression>) -> Result<Expression, String> {
    calc_arithmetic_comparison(">", args, |x, y| x > y)
}

fn proc_ge(args: Vec<Expression>) -> Result<Expression, String> {
    calc_arithmetic_comparison(">=", args, |x, y| x >= y)
}

pub(super) fn define_procs(env: &Env) {
    env.set("+", built_in_proc("+", proc_add));
    env.set("-", built_in_proc("-", proc_sub));
    env.set("*", built_in_proc("*", proc_mul));
    env.set("/", built_in_proc("/", proc_div));

    env.set("=", built_in_proc("=", proc_eq));
    env.set("<", built_in_proc("<", proc_lt));
    env.set("<=", built_in_proc("<=", proc_le));
    env.set(">", built_in_proc(">", proc_gt));
    env.set(">=", built_in_proc(">=", proc_ge));
}

#[cfg(test)]
mod tests {
    use crate::expression::{bool, int, Expression};

    use super::{
        proc_add, proc_div, proc_eq, proc_ge, proc_gt, proc_le, proc_lt, proc_mul, proc_sub,
    };

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
            let values = operands.into_iter().map(int).collect();
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

    #[test]
    fn eval_arithmetic_comparison_test() {
        type Case = (
            fn(Vec<Expression>) -> Result<Expression, String>,
            Vec<i64>,
            bool,
        );
        let cases: Vec<Case> = vec![
            (proc_eq, vec![1, 1, 1], true),
            (proc_eq, vec![1, 1, 2], false),
            (proc_lt, vec![1, 2, 3], true),
            (proc_lt, vec![1, 1, 2], false),
            (proc_lt, vec![1, 3, 2], false),
            (proc_le, vec![1, 1, 1], true),
            (proc_le, vec![1, 1, 2], true),
            (proc_le, vec![1, 2, 3], true),
            (proc_le, vec![1, 2, 1], false),
            (proc_gt, vec![3, 2, 1], true),
            (proc_gt, vec![2, 2, 1], false),
            (proc_gt, vec![3, 1, 2], false),
            (proc_ge, vec![1, 1, 1], true),
            (proc_ge, vec![2, 2, 1], true),
            (proc_ge, vec![3, 2, 1], true),
            (proc_ge, vec![2, 1, 2], false),
        ];
        cases.into_iter().for_each(|(proc, operands, expected)| {
            let values = operands.into_iter().map(int).collect();
            let actual = proc(values).unwrap();
            assert_eq!(actual, bool(expected));
        });
    }

    #[test]
    fn eval_arithmetic_comparison_error_test() {
        type Case = (
            fn(Vec<Expression>) -> Result<Expression, String>,
            Vec<Expression>,
        );
        let cases: Vec<Case> = vec![
            (proc_eq, vec![int(2), bool(false)]),
            (proc_le, vec![]),
            (proc_gt, vec![int(1)]),
        ];
        cases.into_iter().for_each(|(proc, args)| {
            assert!(proc(args).is_err());
        });
    }
}
