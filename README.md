# minsc.rs
Interpreter for a subset of Scheme implemented in Rust.

## Syntax

### Literals
* Booleans: `#t` and `#f`.
* Numerical types: Signed integers (64-bit).
    * No other numerical types are supported (e.g. rational, real, and complex).
* Symbols

### Built-in procedures
* Arithmetic operators: `+`, `-`, `*`, and `/`.
* Numeric comparators: `=`, `<`, `<=`, `>`, and `>=`.

### Special forms
* `define`
* `if`
* `lambda`
* `let`
* `let*`
* `letrec`
