use std::{collections::VecDeque, rc::Rc};

use crate::{
    env::{Env, EnvMaker},
    expression::{closure, symbol, undef, Expression, ExpressionData},
    value::Value,
};

fn as_symbol(expr: &ExpressionData) -> Option<String> {
    if let ExpressionData::Atom(Value::Symbol(symbol)) = expr {
        Some(symbol.to_string())
    } else {
        None
    }
}

fn expect_symbol(expr: &ExpressionData) -> Result<String, String> {
    as_symbol(expr).ok_or("Syntax Error: symbol is expected.".to_string())
}

fn expect_list(expr: Expression) -> Result<Vec<Expression>, String> {
    expr.as_vec()
        .ok_or("Syntax Error: proper list is expected.".to_string())
}

fn expect_symbols(expr: Expression) -> Result<Vec<String>, String> {
    let exprs = expect_list(expr)?;

    let mut symbols = vec![];
    for expr in exprs {
        symbols.push(expect_symbol(expr.as_ref())?);
    }

    Ok(symbols)
}

fn eval_lambda(exprs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
    let mut exprs = VecDeque::from(exprs);

    let params = exprs.pop_front().map_or(
        Err("Syntax Error: malformed labbda".to_string()),
        expect_symbols,
    )?;

    Ok(closure(params, exprs.into(), Rc::clone(env)))
}

fn eval_let(exprs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
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
            let param = expect_symbol(pair.pop().unwrap().as_ref())?;
            inits.push((param, arg));
            Ok(inits)
        })?;

    let extended = env.extend();
    for (param, arg) in inits {
        extended.set(param, eval(arg, env)?);
    }

    exprs
        .into_iter()
        .try_fold(undef(), |_, expr| eval(expr, &extended))
}

fn eval_let_star(exprs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
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
            let param = expect_symbol(pair.pop().unwrap().as_ref())?;
            inits.push((param, arg));
            Ok(inits)
        })?;

    let mut extended = Rc::clone(env);
    for (param, arg) in inits {
        let reducted = eval(arg, &extended)?;
        extended = extended.extend();
        extended.set(param, reducted);
    }

    exprs
        .into_iter()
        .try_fold(undef(), |_, expr| eval(expr, &extended))
}

fn eval_define(exprs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
    if !env.is_top() {
        return Err("Eval Error: define is available only at top-level".to_string());
    }

    let mut exprs = VecDeque::from(exprs);
    match exprs.pop_front() {
        Some(expr) => match expr.as_ref() {
            ExpressionData::Atom(Value::Symbol(name)) => {
                if exprs.len() > 2 {
                    return Err("Syntax Error: malformed define".to_string());
                }

                let reducted = exprs
                    .pop_front()
                    .map(|expr| eval(expr, env))
                    .unwrap_or(Ok(undef()))?;
                env.set(name, reducted);

                Ok(symbol(name))
            }
            ExpressionData::Cons(car, cdr) => {
                let name = expect_symbol(car)?;
                let params = expect_symbols(Rc::clone(cdr))?;
                env.set(&name, closure(params, exprs.into(), Rc::clone(env)));
                Ok(symbol(name))
            }
            _ => Err("Syntax Error: malformed define".to_string()),
        },
        None => Err("Syntax Error: malformed define".to_string()),
    }
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

    let extended = closing.extend();
    for (param, arg) in params.into_iter().zip(args.into_iter()) {
        let value = eval(arg, invocation)?;
        extended.set(param, value);
    }

    body.into_iter()
        .try_fold(undef(), |_, expr| eval(expr, &extended))
}

fn eval_apply(proc: Expression, exprs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
    match eval(proc, env)?.as_ref() {
        ExpressionData::Atom(Value::BuiltInProc { proc, .. }) => {
            proc(eval_expressions(exprs, env)?)
        }
        ExpressionData::Atom(Value::Closure {
            params,
            body,
            env: closing,
        }) => eval_closure(params.clone(), body.clone(), closing, exprs, env),
        _ => Err("Eval Error: Invalid application".to_string()),
    }
}

fn eval_if(exprs: Vec<Expression>, env: &Env) -> Result<Expression, String> {
    if !(2..=3).contains(&exprs.len()) {
        return Err("Syntax Error: malformed if".to_string());
    }

    let mut exprs = VecDeque::from(exprs);
    let predicate = exprs.pop_front().unwrap();
    let consequent = exprs.pop_front().unwrap();

    match eval(predicate, env)?.as_ref() {
        ExpressionData::Atom(Value::Bool(false)) => exprs
            .pop_front()
            .map_or(Ok(undef()), |alternative| eval(alternative, env)),
        _ => eval(consequent, env),
    }
}

fn eval_expressions<T>(exprs: T, env: &Env) -> Result<Vec<Expression>, String>
where
    T: IntoIterator<Item = Expression>,
{
    exprs.into_iter().try_fold(Vec::new(), |mut values, expr| {
        values.push(eval(expr, env)?);
        Ok(values)
    })
}

pub fn eval(expr: Expression, env: &Env) -> Result<Expression, String> {
    match expr.as_ref() {
        ExpressionData::Nil => Ok(expr),
        ExpressionData::Atom(Value::Symbol(symbol)) => env
            .get(symbol)
            .ok_or(format!("Eval Error: undefined variable: {}", symbol)),
        ExpressionData::Atom(_) => Ok(expr),
        ExpressionData::Cons(car, cdr) => {
            let exprs = expect_list(cdr.clone())?;

            match as_symbol(car) {
                Some(symbol) if symbol == "lambda" => eval_lambda(exprs, env),
                Some(symbol) if symbol == "let" => eval_let(exprs, env),
                Some(symbol) if symbol == "let*" => eval_let_star(exprs, env),
                Some(symbol) if symbol == "if" => eval_if(exprs, env),
                Some(symbol) if symbol == "define" => eval_define(exprs, env),
                _ => eval_apply(car.clone(), exprs, env),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::{
        built_in_procs::numbers::define_procs,
        env::{Env, EnvMaker},
        expression::{int, undef},
        parser::parse,
    };

    #[test]
    fn eval_if_test() {
        let env = Env::empty();
        define_procs(&env);
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
            assert_eq!(eval(expr, &env), Ok(expected))
        }

        let cases = vec!["(if #t)", "(if #t 1 2 3)"];
        for input in cases {
            let (_, expr) = parse(input).unwrap();
            assert!(eval(expr, &env).is_err(), "eval({}) should be error", input);
        }
    }

    #[test]
    fn eval_calc_test() {
        let env = Env::empty();
        define_procs(&env);
        let (_, expr) = parse("(+ (*) (/ (- 125) (+ 6 4)) (+ 5))").unwrap();
        let value = eval(expr, &env).unwrap();
        assert_eq!(value, int(-6));
    }

    #[test]
    fn eval_lambda_test() {
        let env = Env::empty();
        define_procs(&env);
        let (_, expr) = parse("((lambda (x y) (+ (* x x) (* y y))) 3 4)").unwrap();
        let value = eval(expr, &env).unwrap();
        assert_eq!(value, int(25));
    }

    #[test]
    fn eval_let_test() {
        let env = Env::empty();
        define_procs(&env);
        let (_, expr) = parse("(let ((a 2) (b 3)) (+ a b))").unwrap();
        let value = eval(expr, &env).unwrap();
        assert_eq!(value, int(5));
    }
}
