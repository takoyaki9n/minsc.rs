use crate::{
    built_in_procs::numbers::{built_in_add, built_in_div, built_in_mul, built_in_sub},
    expression::Expression,
    value::Value,
};

fn eval_symbol(sym: String) -> Result<Value, String> {
    match sym.as_str() {
        "+" => Ok(built_in_add()),
        "-" => Ok(built_in_sub()),
        "*" => Ok(built_in_mul()),
        "/" => Ok(built_in_div()),
        _ => todo!(),
    }
}

fn eval_atom(value: Value) -> Result<Value, String> {
    match value {
        Value::Symbol(sym) => eval_symbol(sym),
        _ => Ok(value),
    }
}

fn eval_if(mut cdrs: Vec<Expression>) -> Result<Value, String> {
    if cdrs.len() != 3 {
        Err(format!("Syntax Error: malformed if."))
    } else {
        let e_else = cdrs.pop().unwrap();
        let e_then = cdrs.pop().unwrap();
        let e_cond = cdrs.pop().unwrap();

        match eval(e_cond)? {
            Value::Bool(false) => eval(e_else),
            _ => eval(e_then),
        }
    }
}

fn eval_args(args: Vec<Expression>) -> Result<Vec<Value>, String> {
    args.into_iter().try_fold(Vec::new(), |mut values, expr| {
        values.push(eval(expr)?);
        Ok(values)
    })
}

fn eval_apply(car: Expression, cdrs: Vec<Expression>) -> Result<Value, String> {
    match eval(car)? {
        Value::BuiltInProc(_, func) => func(eval_args(cdrs)?),
        _ => Err(format!("Eval Error: Invalid application.")),
    }
}

fn opt_symbol(expr: &Expression) -> Option<&str> {
    match expr {
        Expression::Atom(Value::Symbol(sym)) => Some(sym.as_str()),
        _ => None,
    }
}

pub fn eval(expr: Expression) -> Result<Value, String> {
    match expr {
        Expression::Nil => Err(format!("TODO: Eval \"{}\"", expr)),
        Expression::Atom(value) => eval_atom(value),
        Expression::Cons(car, cdr) => {
            let cdrs = cdr
                .flatten()
                .ok_or(format!("Syntax Error: proper list is expected."))?;

            match opt_symbol(&car) {
                Some("if") => eval_if(cdrs),
                _ => eval_apply(*car, cdrs),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::eval;
    use crate::{parser::parse, value::Value};

    #[test]
    fn eval_if_test() {
        let (_, expr) = parse("(if #t 1 2)").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(Value::Int(1), value);

        let (_, expr) = parse("(if #f 1 2)").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(Value::Int(2), value);

        let (_, expr) = parse("(if 0 1 2)").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(Value::Int(1), value);

        let (_, expr) = parse("(if (if #t #f #t) (if #f 1 2) (if #t 3 4))").unwrap();
        let value = eval(expr).unwrap();
        assert_eq!(Value::Int(3), value);
    }

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
            assert_eq!(actual, Value::Int(expected));
        });
    }
}
