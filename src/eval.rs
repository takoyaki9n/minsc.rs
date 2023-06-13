use std::collections::VecDeque;

use crate::{
    built_in_procs::numbers::define_procs,
    env::{top, Env, extend},
    expression::{closure, to_vec, undef, Expression, ExpressionData},
    value::Value,
};

fn opt_symbol(expr: &Expression) -> Option<&str> {
    match expr.as_ref() {
        ExpressionData::Atom(Value::Symbol(symbol)) => Some(symbol),
        _ => None,
    }
}

fn expect_list(expr: Expression) -> Result<Vec<Expression>, String> {
    to_vec(expr).ok_or(format!("Syntax Error: proper list is expected."))
}

fn eval_if(exprs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
    let len = exprs.len();
    if len < 2 || 3 < len {
        return Err(format!("Syntax Error: malformed if"));
    }

    let mut exprs = VecDeque::from(exprs);
    let predicate = exprs.pop_front().unwrap();
    let consequent = exprs.pop_front().unwrap();

    match *eval_expression(predicate, env)? {
        ExpressionData::Atom(Value::Bool(false)) => exprs
            .pop_front()
            .map_or(Ok(undef()), |alternative| eval_expression(alternative, env)),
        _ => eval_expression(consequent, env),
    }
}

fn eval_lambda(exprs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
    let mut exprs = VecDeque::from(exprs);

    let formals = exprs
        .pop_front()
        .map_or(Err(format!("Syntax Error: malformed labbda")), expect_list)?;
    let params = formals
        .into_iter()
        .try_fold(Vec::new(), |mut params, expr| match expr.as_ref() {
            ExpressionData::Atom(Value::Symbol(param)) => {
                params.push(param.to_string());
                Ok(params)
            }
            _ => Err(format!("Syntax Error: string expected")),
        })?;

    Ok(closure(params, exprs.into(), env))
}

fn eval_closure(
    params: Vec<String>,
    body: Vec<Expression>,
    closing: &Env,
    args: Vec<Expression>,
    invocation: &Env,
) -> Result<Expression, String> {
    if params.len() != args.len() {
        return Err(format!(
            "Eval Error: wrong number of arguments: the procedure requires {} but got {}",
            params.len(),
            args.len()
        ));
    }

    let env = extend(closing);
    for (param, arg) in params.into_iter().zip(args.into_iter()) {
        let value = eval_expression(arg, invocation)?;
        env.set(param, value);
    }

    body.into_iter().try_fold(undef(), |_, expr| {
        eval_expression(expr, &env)
    })
}

fn eval_apply(car: Expression, cdrs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
    match eval_expression(car, &env)?.as_ref() {
        ExpressionData::Atom(Value::BuiltInProc { proc, .. }) => {
            proc(eval_expressions(cdrs, &env)?)
        }
        ExpressionData::Atom(Value::Closure {
            params,
            body,
            env: closing,
        }) => eval_closure(params.clone(), body.clone(), closing, cdrs, env),
        _ => Err(format!("Eval Error: Invalid application")),
    }
}

fn eval_expressions<T>(exprs: T, env: &Env) -> Result<Vec<Expression>, String>
where
    T: IntoIterator<Item = Expression>,
{
    exprs.into_iter().try_fold(Vec::new(), |mut values, expr| {
        values.push(eval_expression(expr, env)?);
        Ok(values)
    })
}

fn eval_expression(expr: Expression, env: &Env) -> Result<Expression, String> {
    match expr.as_ref() {
        ExpressionData::Nil => Ok(expr),
        ExpressionData::Atom(Value::Symbol(symbol)) => env
            .get(symbol)
            .ok_or(format!("Eval Error: undefined variable: {}", symbol)),
        ExpressionData::Atom(_) => Ok(expr),
        ExpressionData::Cons(car, cdr) => {
            let exprs = expect_list(cdr.clone())?;

            match opt_symbol(&car) {
                Some("if") => eval_if(exprs, env),
                Some("lambda") => eval_lambda(exprs, env),
                _ => eval_apply(car.clone(), exprs, env),
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
    use crate::{
        expression::{int, undef},
        parser::parse,
    };

    #[test]
    fn eval_if_test() {
        let cases = vec![
            ("(if #t 1 2)", int(1)),
            ("(if #f 1 2)", int(2)),
            ("(if 0 1 2)", int(1)),
            ("(if (if #t #f #t) (if #f 1 2) (if #t 3 4))", int(3)),
            ("(if #t 1)", int(1)),
            ("(if #f 1)", undef()),
        ];
        for (input, expected) in cases {
            let (_, expr) = parse(input).unwrap();
            assert_eq!(eval(expr), Ok(expected))
        }

        let cases = vec!["(if #t)", "(if #t 1 2 3)"];
        for input in cases {
            let (_, expr) = parse(input).unwrap();
            assert!(eval(expr).is_err(), "eval({}) should be error", input);
        }
    }

    #[test]
    fn eval_calc_test() {
        let (_, expr) = parse("(+ (*) (/ (- 125) (+ 6 4)) (+ 5))").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(value, int(-6));
    }

    #[test]
    fn eval_lambda_test() {
        let (_, expr) = parse("((lambda (x y) (+ (* x x) (* y y))) 3 4)").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(value, int(25));
    }
}
