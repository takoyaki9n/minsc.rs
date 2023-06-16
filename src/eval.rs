use std::{collections::VecDeque, rc::Rc};

use crate::{
    built_in_procs::numbers::define_procs,
    env::{extend, top, Env},
    expression::{closure, undef, Expression, ExpressionData},
    value::Value,
};

fn opt_symbol(expr: &ExpressionData) -> Option<&str> {
    if let ExpressionData::Atom(Value::Symbol(symbol)) = expr {
        Some(symbol.as_str())
    } else {
        None
    }
}

fn expect_symbol(expr: Expression) -> Result<String, String> {
    match expr.as_ref() {
        ExpressionData::Atom(Value::Symbol(symbol)) => Ok(symbol.clone()),
        _ => Err("Syntax Error: proper list is expected.".to_string()),
    }
}

fn expect_list(expr: Expression) -> Result<Vec<Expression>, String> {
    expr.to_vec()
        .ok_or("Syntax Error: proper list is expected.".to_string())
}

fn eval_lambda(exprs: Vec<Expression>, env: Env) -> Result<Expression, String> {
    let mut exprs = VecDeque::from(exprs);

    let formals = exprs.pop_front().map_or(
        Err("Syntax Error: malformed labbda".to_string()),
        expect_list,
    )?;
    let params = formals.into_iter().try_fold(
        vec![],
        |mut params, expr| -> Result<Vec<String>, String> {
            params.push(expect_symbol(expr)?);
            Ok(params)
        },
    )?;

    Ok(closure(params, exprs.into(), Rc::clone(&env)))
}

fn eval_let(exprs: Vec<Expression>, env: Env) -> Result<Expression, String> {
    let mut exprs = VecDeque::from(exprs);

    let inits = exprs
        .pop_front()
        .map_or(Err("Syntax Error: malformed let".to_string()), expect_list)?
        .into_iter()
        .try_fold(vec![], |mut inits, expr| {
            let mut pair = expect_list(expr)?;
            if pair.len() != 2 {
                return Err("Syntax Error: malformed let".to_string());
            }

            let arg = pair.pop().unwrap();
            let param = expect_symbol(pair.pop().unwrap())?;
            inits.push((param, arg));
            Ok(inits)
        })?;

    let extended = extend(Rc::clone(&env));
    for (param, arg) in inits {
        extended.set(param, eval_expression(arg, Rc::clone(&env))?);
    }

    exprs.into_iter().try_fold(undef(), |_, expr| {
        eval_expression(expr, Rc::clone(&extended))
    })
}

fn eval_closure(
    params: Vec<String>,
    body: Vec<Expression>,
    closing: Env,
    args: Vec<Expression>,
    invocation: Env,
) -> Result<Expression, String> {
    if params.len() != args.len() {
        return Err(format!(
            "Eval Error: wrong number of arguments: the procedure requires {} but got {}",
            params.len(),
            args.len()
        ));
    }

    let extended = extend(closing);
    for (param, arg) in params.into_iter().zip(args.into_iter()) {
        let value = eval_expression(arg, Rc::clone(&invocation))?;
        extended.set(param, value);
    }

    body.into_iter().try_fold(undef(), |_, expr| {
        eval_expression(expr, Rc::clone(&extended))
    })
}

fn eval_apply(proc: Expression, exprs: Vec<Expression>, env: Env) -> Result<Expression, String> {
    match eval_expression(proc, Rc::clone(&env))?.as_ref() {
        ExpressionData::Atom(Value::BuiltInProc { proc, .. }) => {
            proc(eval_expressions(exprs, env)?)
        }
        ExpressionData::Atom(Value::Closure {
            params,
            body,
            env: closing,
        }) => eval_closure(params.clone(), body.clone(), Rc::clone(closing), exprs, env),
        _ => Err("Eval Error: Invalid application".to_string()),
    }
}

fn eval_if(exprs: Vec<Expression>, env: Env) -> Result<Expression, String> {
    if !(2..=3).contains(&exprs.len()) {
        return Err("Syntax Error: malformed if".to_string());
    }

    let mut exprs = VecDeque::from(exprs);
    let predicate = exprs.pop_front().unwrap();
    let consequent = exprs.pop_front().unwrap();

    match eval_expression(predicate, Rc::clone(&env))?.as_ref() {
        ExpressionData::Atom(Value::Bool(false)) => exprs
            .pop_front()
            .map_or(Ok(undef()), |alternative| eval_expression(alternative, env)),
        _ => eval_expression(consequent, env),
    }
}

fn eval_expressions<T>(exprs: T, env: Env) -> Result<Vec<Expression>, String>
where
    T: IntoIterator<Item = Expression>,
{
    exprs.into_iter().try_fold(Vec::new(), |mut values, expr| {
        values.push(eval_expression(expr, Rc::clone(&env))?);
        Ok(values)
    })
}

fn eval_expression(expr: Expression, env: Env) -> Result<Expression, String> {
    match expr.as_ref() {
        ExpressionData::Nil => Ok(expr),
        ExpressionData::Atom(Value::Symbol(symbol)) => env
            .get(symbol)
            .ok_or(format!("Eval Error: undefined variable: {}", symbol)),
        ExpressionData::Atom(_) => Ok(expr),
        ExpressionData::Cons(car, cdr) => {
            let exprs = expect_list(cdr.clone())?;

            match opt_symbol(car) {
                Some("lambda") => eval_lambda(exprs, env),
                Some("let") => eval_let(exprs, env),
                Some("if") => eval_if(exprs, env),
                _ => eval_apply(car.clone(), exprs, env),
            }
        }
    }
}

pub fn eval(expr: Expression) -> Result<Expression, String> {
    let env = top();
    define_procs(&env);

    eval_expression(expr, env)
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

    #[test]
    fn eval_let_test() {
        let (_, expr) = parse("(let ((a 2) (b 3)) (+ a b))").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(value, int(5));
    }
}
