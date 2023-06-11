use crate::{
    built_in_procs::numbers::define_procs,
    env::{top, Env},
    expression::Expression,
    value::{Value, ValueData},
};

fn eval_atom(value: Value, env: &Env) -> Result<Value, String> {
    match value.as_ref() {
        ValueData::Symbol(sym) => env
            .get(sym)
            .ok_or(format!("Eval Error: undefined variable: {}", sym)),
        _ => Ok(value.clone()),
    }
}

fn eval_if(mut cdrs: Vec<Expression>, env: &Env) -> Result<Value, String> {
    if cdrs.len() != 3 {
        Err(format!("Syntax Error: malformed if."))
    } else {
        let e_else = cdrs.pop().unwrap();
        let e_then = cdrs.pop().unwrap();
        let e_cond = cdrs.pop().unwrap();

        match *eval_expression(e_cond, env)? {
            ValueData::Bool(false) => eval_expression(e_else, env),
            _ => eval_expression(e_then, env),
        }
    }
}

fn eval_expressions(exprs: Vec<Expression>, env: &Env) -> Result<Vec<Value>, String> {
    exprs.into_iter().try_fold(Vec::new(), |mut values, expr| {
        values.push(eval_expression(expr, env)?);
        Ok(values)
    })
}

fn eval_apply(car: Expression, cdrs: Vec<Expression>, env: &Env) -> Result<Value, String> {
    match *eval_expression(car, &env)? {
        ValueData::BuiltInProc { proc, .. } => proc(eval_expressions(cdrs, &env)?),
        _ => Err(format!("Eval Error: Invalid application.")),
    }
}

fn opt_symbol(expr: &Expression) -> Option<&str> {
    match expr {
        Expression::Atom(value) => match value.as_ref() {
            ValueData::Symbol(sym) => Some(sym.as_str()),
            _ => None,
        },
        _ => None,
    }
}

pub fn eval_expression(expr: Expression, env: &Env) -> Result<Value, String> {
    match expr {
        Expression::Nil => Err(format!("TODO: Eval \"{}\"", expr)),
        Expression::Atom(value) => eval_atom(value, env),
        Expression::Cons(car, cdr) => {
            let cdrs = cdr
                .to_vec()
                .ok_or(format!("Syntax Error: proper list is expected."))?;

            match opt_symbol(&car) {
                Some("if") => eval_if(cdrs, env),
                _ => eval_apply(*car, cdrs, env),
            }
        }
    }
}

pub fn eval(expr: Expression) -> Result<Value, String> {
    let env = top();
    define_procs(&env);

    eval_expression(expr, &env)
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::{parser::parse, value::int};

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
