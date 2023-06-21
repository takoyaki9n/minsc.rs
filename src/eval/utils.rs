use crate::expression::{Expression, ExpressionConverter};

pub(super) fn expect_number(expr: &Expression) -> Result<i64, String> {
    expr.as_number()
        .map_err(|_| format!("Type Error: number is expected: {}", expr))
}

pub(super) fn expect_symbol(expr: &Expression) -> Result<String, String> {
    expr.as_symbol()
        .map_err(|_| format!("Type Error: symbol is expected: {}", expr))
}

pub(super) fn expect_list(expr: &Expression) -> Result<Vec<Expression>, String> {
    expr.as_vec()
        .map_err(|_| format!("Syntax Error: proper list is expected: {}", expr))
}

pub(super) fn try_map_expressions<T, E, F>(exprs: &[Expression], f: F) -> Result<Vec<T>, E>
where
    F: Fn(&Expression) -> Result<T, E>,
{
    exprs.iter().try_fold(vec![], |mut acc, expr| {
        acc.push(f(expr)?);
        Ok(acc)
    })
}

pub(super) fn expect_numbers(exprs: &[Expression]) -> Result<Vec<i64>, String> {
    try_map_expressions(exprs, expect_number)
}

pub(super) fn expect_symbols(exprs: &[Expression]) -> Result<Vec<String>, String> {
    try_map_expressions(exprs, expect_symbol)
}
