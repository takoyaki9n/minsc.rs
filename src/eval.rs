use std::collections::VecDeque;

use crate::{
    built_in_procs::numbers::define_procs,
    env::{top, Env},
    expression::{to_vec, undef, Expression, ExpressionData},
    value::Value,
};

fn eval_symbol(symbol: &str, env: &Env) -> Result<Expression, String> {
    env.get(symbol)
        .ok_or(format!("Eval Error: undefined variable: {}", symbol))
}

fn eval_if(exprs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
    if exprs.len() == 2 || exprs.len() == 3 {
        let mut exprs = VecDeque::from(exprs);
        let predicate = exprs.pop_front().unwrap();
        let consequent = exprs.pop_front().unwrap();

        match *eval_expression(predicate, env)? {
            ExpressionData::Atom(Value::Bool(false)) => exprs
                .pop_front()
                .map_or(Ok(undef()), |alternative| eval_expression(alternative, env)),
            _ => eval_expression(consequent, env),
        }
    } else {
        Err(format!("Syntax Error: malformed if"))
    }
}

fn eval_expressions(exprs: Vec<Expression>, env: &Env) -> Result<Vec<Expression>, String> {
    exprs.into_iter().try_fold(Vec::new(), |mut values, expr| {
        values.push(eval_expression(expr, env)?);
        Ok(values)
    })
}

fn eval_apply(car: Expression, cdrs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
    match *eval_expression(car, &env)? {
        ExpressionData::Atom(Value::BuiltInProc { proc, .. }) => {
            proc(eval_expressions(cdrs, &env)?)
        }
        _ => Err(format!("Eval Error: Invalid application.")),
    }
}

fn opt_symbol(expr: &Expression) -> Option<&str> {
    match expr.as_ref() {
        ExpressionData::Atom(Value::Symbol(symbol)) => Some(symbol),
        _ => None,
    }
}

pub fn eval_expression(expr: Expression, env: &Env) -> Result<Expression, String> {
    match expr.as_ref() {
        ExpressionData::Nil => Ok(expr),
        ExpressionData::Atom(Value::Symbol(symbol)) => eval_symbol(symbol.as_str(), env),
        ExpressionData::Atom(_) => Ok(expr),
        ExpressionData::Cons(car, cdr) => {
            let cdrs = to_vec(cdr.clone()).ok_or(format!("Syntax Error: proper list is expected."))?;

            match car.as_ref() {
                ExpressionData::Atom(Value::Symbol(s)) if s == "if" => eval_if(cdrs, env),
                _ => eval_apply(car.clone(), cdrs, env),
            }
        }
    }
}

pub fn eval(expr: Expression) -> Result<Expression, String> {
    let env = top();
    define_procs(&env);

    eval_expression(expr, &env)
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::{parser::parse, expression::int};

    #[test]
    fn eval_if_test() {
        let (_, expr) = parse("(if #t 1 2)").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(value, int(1));

        let (_, expr) = parse("(if #f 1 2)").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(value, int(2));

        let (_, expr) = parse("(if 0 1 2)").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(value, int(1));

        let (_, expr) = parse("(if (if #t #f #t) (if #f 1 2) (if #t 3 4))").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(value, int(3));
    }

    #[test]
    fn eval_calc_test() {
        let (_, expr) = parse("(+ (*) (/ (- 125) (+ 6 4)) (+ 5))").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(value, int(-6));
    }
}
