use std::rc::Rc;

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

fn expect_list(expr: &Expression) -> Result<Vec<Expression>, String> {
    expr.as_vec()
        .ok_or("Syntax Error: proper list is expected.".to_string())
}

fn expect_symbols(expr: &Expression) -> Result<Vec<String>, String> {
    let exprs = expect_list(expr)?;

    let mut symbols = vec![];
    for expr in exprs {
        symbols.push(expect_symbol(expr.as_ref())?);
    }

    Ok(symbols)
}


fn map_eval(exprs: &[Expression], env: &Env) -> Result<Vec<Expression>, String> {
    exprs.iter().try_fold(Vec::new(), |mut values, expr| {
        values.push(eval_expression(expr, env)?);
        Ok(values)
    })
}

fn eval_lambda(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    let mut exprs = exprs.iter();

    let params = exprs.next().map_or(
        Err("Syntax Error: malformed labbda".to_string()),
        expect_symbols,
    )?;

    let body = exprs.map(Rc::clone).collect();

    Ok(closure(params, body, Rc::clone(env)))
}

fn eval_let(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    let mut exprs = exprs.iter();

    let inits = exprs
        .next()
        .map_or(Err("Syntax Error: malformed let".to_string()), expect_list)?
        .iter()
        .try_fold(vec![], |mut inits, expr| {
            let mut pair = expect_list(expr)?.into_iter();
            if pair.len() != 2 {
                return Err("Syntax Error: malformed let".to_string());
            }

            let param = expect_symbol(pair.next().unwrap().as_ref())?;
            let arg = pair.next().unwrap();
            inits.push((param, arg));
            Ok(inits)
        })?;

    let extended = env.extend();
    for (param, arg) in inits {
        extended.set(param, eval(arg, env)?);
    }

    exprs.try_fold(undef(), |_, expr| eval_expression(expr, &extended))
}

fn eval_let_star(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    let mut exprs = exprs.iter();

    let inits = exprs
        .next()
        .map_or(Err("Syntax Error: malformed let".to_string()), expect_list)?
        .iter()
        .try_fold(vec![], |mut inits, expr| {
            let mut pair = expect_list(expr)?.into_iter();
            if pair.len() != 2 {
                return Err("Syntax Error: malformed let".to_string());
            }

            let param = expect_symbol(pair.next().unwrap().as_ref())?;
            let arg = pair.next().unwrap();
            inits.push((param, arg));
            Ok(inits)
        })?;

    let mut extended = Rc::clone(env);
    for (param, arg) in inits {
        let evaled = eval(arg, &extended)?;
        extended = extended.extend();
        extended.set(param, evaled);
    }

    exprs.try_fold(undef(), |_, expr| eval_expression(expr, &extended))
}

fn eval_define(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    if !env.is_top() {
        return Err("Eval Error: define is available only at top-level".to_string());
    }

    let mut exprs = exprs.iter();
    let (name, evaled) = match exprs.next().map(Rc::as_ref) {
        Some(ExpressionData::Atom(Value::Symbol(name))) => {
            if exprs.len() > 2 {
                return Err("Syntax Error: malformed define".to_string());
            }

            let evaled = exprs
                .next()
                .map_or(Ok(undef()), |expr| eval_expression(expr, env))?;

            Ok((name.to_string(), evaled))
        }
        Some(ExpressionData::Cons(car, cdr)) => {
            let name = expect_symbol(car)?;
            let params = expect_symbols(cdr)?;
            let body = exprs.map(Rc::clone).collect();

            Ok((name, closure(params, body, Rc::clone(env))))
        }
        _ => Err("Syntax Error: malformed define".to_string()),
    }?;
    env.set(&name, evaled);

    Ok(symbol(name))
}

fn eval_closure(
    params: &[String],
    body: &[Expression],
    closing: &Env,
    args: &[Expression],
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
    for (param, arg) in params.iter().zip(args.iter()) {
        extended.set(param, eval(Rc::clone(arg), invocation)?);
    }

    body.iter()
        .try_fold(undef(), |_, expr| eval_expression(expr, &extended))
}

fn eval_apply(proc: &Expression, exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    match eval_expression(proc, env)?.as_ref() {
        ExpressionData::Atom(Value::BuiltInProc { proc, .. }) => proc(map_eval(exprs, env)?),
        ExpressionData::Atom(Value::Closure {
            params,
            body,
            env: closing,
        }) => eval_closure(params, body, closing, exprs, env),
        _ => Err("Eval Error: Invalid application".to_string()),
    }
}

fn eval_if(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    if !(2..=3).contains(&exprs.len()) {
        return Err("Syntax Error: malformed if".to_string());
    }

    let mut exprs = exprs.iter();
    let predicate = exprs.next().unwrap();
    let consequent = exprs.next().unwrap();

    match eval_expression(predicate, env)?.as_ref() {
        ExpressionData::Atom(Value::Bool(false)) => exprs
            .next()
            .map_or(Ok(undef()), |alternative| eval_expression(alternative, env)),
        _ => eval_expression(consequent, env),
    }
}

pub fn eval_expression(expr: &Expression, env: &Env) -> Result<Expression, String> {
    match expr.as_ref() {
        ExpressionData::Atom(Value::Symbol(symbol)) => env
            .get(symbol)
            .ok_or(format!("Eval Error: undefined variable: {}", symbol)),
        ExpressionData::Cons(car, cdr) => {
            let exprs = expect_list(cdr)?;

            match as_symbol(car) {
                Some(symbol) if symbol == "lambda" => eval_lambda(&exprs, env),
                Some(symbol) if symbol == "let" => eval_let(&exprs, env),
                Some(symbol) if symbol == "let*" => eval_let_star(&exprs, env),
                Some(symbol) if symbol == "define" => eval_define(&exprs, env),
                Some(symbol) if symbol == "if" => eval_if(&exprs, env),
                _ => eval_apply(car, &exprs, env),
            }
        }
        _ => Ok(Rc::clone(expr)),
    }
}

pub fn eval(expr: Expression, env: &Env) -> Result<Expression, String> {
    eval_expression(&expr, env)
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::{
        built_in_procs::numbers::define_procs,
        env::{Env, EnvMaker},
        expression::{bool, int, undef},
        parser::parse,
    };

    macro_rules! assert_eval_ok {
        ($code: literal, $expected: expr) => {
            let env = Env::empty();
            define_procs(&env);
            let (_, expr) = parse($code).unwrap();
            assert_eq!(eval(expr, &env), Ok($expected), "{}", $code);
        };
    }
    macro_rules! assert_eval_err {
        ($code: literal) => {
            let env = Env::empty();
            define_procs(&env);
            let (_, expr) = parse($code).unwrap();
            assert!(eval(expr, &env).is_err(), "{}", $code);
        };
    }

    #[test]
    fn eval_if_test() {
        assert_eval_ok!("(if #t 1 2)", int(1));
        assert_eval_ok!("(if #f 1 2)", int(2));
        assert_eval_ok!("(if 0 1 2)", int(1));
        assert_eval_ok!("(if (if #t #f #t) (if #f 1 2) (if #t 3 4))", int(3));
        assert_eval_ok!("(if #t 1)", int(1));
        assert_eval_ok!("(if #f 1)", undef());

        assert_eval_err!("(if #t)");
        assert_eval_err!("(if #t 1 2 3)");
    }

    #[test]
    fn eval_calc_test() {
        assert_eval_ok!("(+ (*) (/ (- 125) (+ 6 4)) (+ 5))", int(-6));
    }

    #[test]
    fn eval_lambda_test() {
        assert_eval_ok!("((lambda (x y) (+ (* x x) (* y y))) 3 4)", int(25));
        assert_eval_ok!("((lambda () #t))", bool(true));
        assert_eval_ok!("((lambda (x)) 1)", undef());

        assert_eval_err!("((lambda x (+ x 1)) 10)");
        assert_eval_err!("((lambda (x) x) 1 2)");
        assert_eval_err!("((lambda (x y) (- x y)) 1)");
        assert_eval_err!("((lambda (1 x) x) 1 2)");
        assert_eval_err!("((lambda (x . y) y) 1 2)");
    }

    #[test]
    fn eval_let_test() {
        assert_eval_ok!("(let ((a 2) (b (/ 10 2))) (+ a b))", int(7));
        assert_eval_ok!("(let* ((a 2) (b (/ 10 2))) (+ a b))", int(7));

        assert_eval_ok!("(let () 12345)", int(12345));
        assert_eval_ok!("(let* () 12345)", int(12345));

        assert_eval_ok!("(let ((a 1) (b 2)) (* 3 4) (+ a b))", int(3));
        assert_eval_ok!("(let* ((a 1) (b 2)) (* 3 4) (+ a b))", int(3));

        assert_eval_ok!("(let ((a 1) (b 2)) (let ((a 3)) (* a b)))", int(6));
        assert_eval_ok!("(let* ((a 1) (b 2)) (let* ((a 3)) (* a b)))", int(6));

        assert_eval_err!("(let ((a 2) (b (+ a 3))) (* a b))");
        assert_eval_ok!("(let* ((a 2) (b (+ a 3))) (* a b))", int(10));

        assert_eval_err!("(let ((a 1) (b (- 5 a)) (a 3)) (* a b))");
        assert_eval_ok!("(let* ((a 1) (b (- 5 a)) (a 3)) (* a b))", int(12));

        assert_eval_ok!(
            "(let ((fix (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y)))))))
                   (fact (lambda (f) (lambda (n) (if (< n 2) 1 (* n (f (- n 1)))))))) 
                ((fix fact) 6))",
            int(720)
        );
        assert_eval_ok!(
            "(let* ((fix (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y)))))))
                   (fact (lambda (f) (lambda (n) (if (< n 2) 1 (* n (f (- n 1)))))))) 
                ((fix fact) 6))",
            int(720)
        );

        assert_eval_err!(
            "(let ((fix (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y)))))))
                    (fact (fix (lambda (f) (lambda (n) (if (< n 2) 1 (* n (f (- n 1)))))))))
                (fact 6))"
        );
        assert_eval_ok!(
            "(let* ((fix (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y)))))))
                    (fact (fix (lambda (f) (lambda (n) (if (< n 2) 1 (* n (f (- n 1)))))))))
                (fact 6))",
            int(720)
        );

        assert_eval_err!(
            "(let ((fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1)))))))
                (fact 5))"
        );
        assert_eval_err!(
            "(let* ((fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1)))))))
                (fact 5))"
        );

        assert_eval_err!("(let (+ 1 2))");
        assert_eval_err!("(let* (+ 1 2))");

        assert_eval_err!("(let x (* x 2))");
        assert_eval_err!("(let* x (* x 2))");

        assert_eval_err!("(let (x 1) (+ x 2))");
        assert_eval_err!("(let* (x 1) (+ x 2))");

        assert_eval_err!("(let ((a . 1)) (+ a 2))");
        assert_eval_err!("(let* ((a . 1)) (+ a 2))");

        assert_eval_err!("(let ((a 1) . (b 2)) (+ a b))");
        assert_eval_err!("(let* ((a 1) . (b 2)) (+ a b))");
    }

    #[test]
    fn define_test() {
        let cases = vec![
            (vec!["(define a 10)", "(+ a 4)"], int(14)),
            (vec!["(define a 10)", "(let ((a 12)) (- 100 a))"], int(88)),
            (
                vec!["(define a 10)", "(let ((a 12)) (- 100 a))", "a"],
                int(10),
            ),
            (vec!["(define (f x) (+ 1 2) (* x x))", "(f 10)"], int(100)),
            (
                vec![
                    "(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))",
                    "(fact 5)",
                ],
                int(120),
            ),
        ];
        for (codes, expected) in cases {
            let env = Env::empty();
            define_procs(&env);
            let actual = codes.iter().fold(Ok(undef()), |_, code| {
                let (_, expr) = parse(code).unwrap();
                eval(expr, &env)
            });
            assert_eq!(actual, Ok(expected), "{}", codes.join("\n"));
        }
    }
}
