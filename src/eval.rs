use crate::{expression::Expression, parser::parse, value::Value};

fn eval_atom(value: Value) -> Result<Value, String> {
    match value {
        Value::Symbol(sym) => Err(format!("TODO: Eval \"{}\".", sym)),
        _ => Ok(value),
    }
}

fn eval_if(expr: Expression) -> Result<Value, String> {
    let mut exprs = expr
        .flatten()
        .ok_or(format!("Syntax Error: proper list is expected for if form"))?;

    if exprs.len() != 3 {
        Err(format!("Syntax Error: malformed if."))
    } else {
        let e_else = exprs.pop().unwrap();
        let e_then = exprs.pop().unwrap();
        let e_cond = exprs.pop().unwrap();

        match eval(e_cond)? {
            Value::Bool(false) => eval(e_else),
            _ => eval(e_then),
        }
    }
}

pub fn eval(expr: Expression) -> Result<Value, String> {
    match expr {
        Expression::Nil => Err(format!("TODO: Eval \"{}\"", expr)),
        Expression::Atom(value) => eval_atom(value),
        Expression::Cons(car, cdr) => {
            if let Expression::Atom(Value::Symbol(sym)) = *car {
                match sym.as_str() {
                    "if" => eval_if(*cdr),
                    _ => todo!(),
                }
            } else {
                todo!()
            }
        }
    }
}

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
}
