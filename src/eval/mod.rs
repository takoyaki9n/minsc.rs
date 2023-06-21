mod built_in_procs;
mod utils;

use std::rc::Rc;

use crate::{
    env::{Env, EnvMaker},
    expression::{
        closure, special_form, symbol, undef, Expression,
        ExpressionInner::{Atom, Cons},
    },
    value::Value::{Bool, BuiltInProc, Closure, SpecialForm, Symbol},
};

use self::{
    built_in_procs::define_procs,
    utils::{expect_list, expect_symbol, expect_symbols, try_map_expressions},
};

fn eval_expressions(exprs: &[Expression], env: &Env) -> Result<Vec<Expression>, String> {
    try_map_expressions(exprs, |expr| eval_expression(expr, env))
}

fn eval_define(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    if !env.is_top() {
        return Err("Eval Error: define is available only at top-level".to_string());
    }

    let mut exprs = exprs.iter();
    let (name, evaled) = match exprs.next().map(Rc::as_ref) {
        Some(Atom(Symbol(name))) => {
            if exprs.len() > 2 {
                return Err("Syntax Error: malformed define".to_string());
            }

            let evaled = exprs
                .next()
                .map_or(Ok(undef()), |expr| eval_expression(expr, env))?;

            Ok((name.to_string(), evaled))
        }
        Some(Cons(car, cdr)) => {
            let name = expect_symbol(car)?;
            let params = expect_list(cdr).and_then(|cdrs| expect_symbols(&cdrs))?;
            let body = exprs.map(Rc::clone).collect();

            Ok((name, closure(params, body, Rc::clone(env))))
        }
        _ => Err("Syntax Error: malformed define".to_string()),
    }?;
    env.set(&name, evaled);

    Ok(symbol(name))
}

fn eval_if(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    if !(2..=3).contains(&exprs.len()) {
        return Err("Syntax Error: malformed if".to_string());
    }

    let mut exprs = exprs.iter();
    let predicate = exprs.next().unwrap();
    let consequent = exprs.next().unwrap();

    match eval_expression(predicate, env)?.as_ref() {
        Atom(Bool(false)) => exprs
            .next()
            .map_or(Ok(undef()), |alternative| eval_expression(alternative, env)),
        _ => eval_expression(consequent, env),
    }
}

fn eval_lambda(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    let mut exprs = exprs.iter();

    let params = exprs
        .next()
        .ok_or("Syntax Error: malformed lambda".to_string())
        .and_then(|expr| expect_symbols(&expect_list(expr)?))?;

    let body = exprs.map(Rc::clone).collect();

    Ok(closure(params, body, Rc::clone(env)))
}

#[derive(Debug, PartialEq, Eq)]
enum ClosureVariant<'a> {
    Closure(&'a Env),
    Let,
    LetStar,
    LetRec,
}

fn eval_closure_variant(
    inits: &[(String, Expression)],
    body: &[Expression],
    env: &Env,
    variant: ClosureVariant,
) -> Result<Expression, String> {
    use ClosureVariant::*;

    let mut env_body = match variant {
        LetStar => Rc::clone(env),
        _ => env.extend(),
    };
    let mut env_arg = Rc::clone(match variant {
        Closure(invocation) => invocation,
        Let => env,
        _ => &env_body,
    });
    for (param, arg) in inits {
        if variant == LetStar {
            env_arg = Rc::clone(&env_body);
            env_body = env_body.extend();
        }
        let evaled = eval_expression(arg, &env_arg)?;
        env_body.set(param, evaled);
    }

    body.iter()
        .try_fold(undef(), |_, expr| eval_expression(expr, &env_body))
}

fn eval_let_variant(
    exprs: &[Expression],
    env: &Env,
    variant: ClosureVariant,
) -> Result<Expression, String> {
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

            let param = expect_symbol(&pair.next().unwrap())?;
            let arg = pair.next().unwrap();
            inits.push((param, arg));
            Ok(inits)
        })?;

    let body = exprs.map(Rc::clone).collect::<Vec<_>>();

    eval_closure_variant(&inits, &body, env, variant)
}

fn eval_let(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    eval_let_variant(exprs, env, ClosureVariant::Let)
}

fn eval_let_star(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    eval_let_variant(exprs, env, ClosureVariant::LetStar)
}

fn eval_letrec(exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    eval_let_variant(exprs, env, ClosureVariant::LetRec)
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

    let params = params.iter().map(String::to_string);
    let args = args.iter().map(Rc::clone);
    let inits = params.zip(args).collect::<Vec<_>>();

    eval_closure_variant(&inits, body, closing, ClosureVariant::Closure(invocation))
}

fn eval_apply(proc: &Expression, exprs: &[Expression], env: &Env) -> Result<Expression, String> {
    match eval_expression(proc, env)?.as_ref() {
        Atom(SpecialForm { eval, .. }) => eval(exprs, env),
        Atom(BuiltInProc { proc, .. }) => {
            let args = eval_expressions(exprs, env)?;
            proc(&args)
        }
        Atom(Closure {
            params,
            body,
            env: closing,
        }) => eval_closure(params, body, closing, exprs, env),
        _ => Err("Eval Error: Invalid application".to_string()),
    }
}

fn eval_expression(expr: &Expression, env: &Env) -> Result<Expression, String> {
    match expr.as_ref() {
        Atom(Symbol(name)) => env
            .get(name)
            .ok_or(format!("Eval Error: undefined variable: {}", &name)),
        Cons(car, cdr) => {
            let args = expect_list(cdr)?;
            eval_apply(car, &args, env)
        }
        _ => Ok(Rc::clone(expr)),
    }
}

pub(crate) struct Interpreter {
    env: Env,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter { env: Env::empty() }
    }

    fn define_special_forms(&self) {
        type EvalFn = fn(&[Expression], &Env) -> Result<Expression, String>;
        let forms: [(&str, EvalFn); 6] = [
            ("define", eval_define),
            ("if", eval_if),
            ("lambda", eval_lambda),
            ("let", eval_let),
            ("let*", eval_let_star),
            ("letrec", eval_letrec),
        ];
        for (name, eval) in forms {
            self.env.set(name, special_form(name, eval));
        }
    }

    pub fn init(&self) {
        self.define_special_forms();
        define_procs(&self.env)
    }

    pub fn eval(&self, expr: Expression) -> Result<Expression, String> {
        eval_expression(&expr, &self.env)
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;
    use crate::{
        expression::{bool, int, undef},
        parser::parse,
    };

    macro_rules! assert_eval_ok {
        ($code: expr, $expected: expr) => {{
            let interpreter = Interpreter::new();
            interpreter.init();
            let (_, expr) = parse($code).unwrap();
            assert_eq!(interpreter.eval(expr), Ok($expected), "{}", $code);
        }};
    }
    macro_rules! assert_eval_err {
        ($code: expr) => {{
            let interpreter = Interpreter::new();
            interpreter.init();
            let (_, expr) = parse($code).unwrap();
            assert!(interpreter.eval(expr).is_err(), "{}", $code);
        }};
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
        let names = ["let", "let*", "letrec"];
        let cases = [
            (
                "(let ((a 2) (b (/ 10 2))) (+ a b))",
                [Ok(int(7)), Ok(int(7)), Ok(int(7))],
            ),
            (
                "(let () 12345)",
                [Ok(int(12345)), Ok(int(12345)), Ok(int(12345))],
            ),
            (
                "(let ((a 1) (b 2)) (* 3 4) (+ a b))",
                [Ok(int(3)), Ok(int(3)), Ok(int(3))],
            ),
            (
                "(let ((a 1) (b 2)) (let ((a 3)) (* a b)))",
                [Ok(int(6)), Ok(int(6)), Ok(int(6))],
            ),
            (
                "(let ((a 2) (b (+ a 3))) (* a b))",
                [Err(()), Ok(int(10)), Ok(int(10))]
            ),
            (
                "(let ((a 1) (b 4) (a 3)) (* a b))",
                [Ok(int(12)), Ok(int(12)), Ok(int(12))] // Undefined behavior for `let` and `letrec`
            ),
            (
                "(let ((fix (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y)))))))
                       (fact (lambda (f) (lambda (n) (if (< n 2) 1 (* n (f (- n 1))))))))
                    ((fix fact) 6))",
                [Ok(int(720)), Ok(int(720)), Ok(int(720))]
            ),
            (
                "(let ((fix (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y)))))))
                       (fact (fix (lambda (f) (lambda (n) (if (< n 2) 1 (* n (f (- n 1)))))))))
                    (fact 6))",
                [Err(()), Ok(int(720)), Ok(int(720))]
            ),
            (
                "(let ((fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1)))))))
                    (fact 5))",
                [Err(()), Err(()), Ok(int(120))]
            ),
            (
                "(let ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) 
                          (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
                    (even? 11))",
                    [Err(()), Err(()), Ok(bool(false))]
            ),
            ("(let (+ 1 2))", [Err(()), Err(()), Err(())]),
            ("(let x (* x 2))", [Err(()), Err(()), Err(())]),
            ("(let (x 1) (+ x 2))", [Err(()), Err(()), Err(())]),
            ("(let ((a . 1)) (+ a 2))", [Err(()), Err(()), Err(())]),
            ("(let ((a 1) . (b 2)) (+ a b))", [Err(()), Err(()), Err(())]),
        ];
        for (code, expects) in cases {
            for (name, expect) in names.into_iter().zip(expects.into_iter()) {
                let code = code.replace("let", name);
                match expect {
                    Ok(expected) => assert_eval_ok!(code.as_str(), expected),
                    Err(()) => assert_eval_err!(code.as_str()),
                }
            }
        }
    }

    #[test]
    fn define_test() {
        let cases = [
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
            let interpreter = Interpreter::new();
            interpreter.init();
            let actual = codes.iter().fold(Ok(undef()), |_, code| {
                let (_, expr) = parse(code).unwrap();
                interpreter.eval(expr)
            });
            assert_eq!(actual, Ok(expected), "{}", codes.join("\n"));
        }
    }
}
