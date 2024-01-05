# minsc.rs
Interpreter of a subset of Scheme implemented in Rust.

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
#### `define`
```
minsc.rs> (define (f x) (+ x x))
f
minsc.rs> (define a 10)
a
minsc.rs> (f a)
20
```

#### `if`
```
minsc.rs> (if (< 1 2) 3 4)
3
```

#### `lambda`
```
minsc.rs> ((lambda (x y) (+ (* x x) (* y y))) 3 4)
25
```

#### `let`
```
minsc.rs> (let 
    ((fix (lambda (f) ((lambda (x) (f (lambda (y) ((x x) y)))) (lambda (x) (f (lambda (y) ((x x) y))))))) 
     (fact (lambda (f) (lambda (n) (if (< n 2) 1 (* n (f (- n 1)))))))) 
        ((fix fact) 4))
24
```

#### `letrec`
```
minsc.ts> (letrec 
    ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
     (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) 
        (even? 11))
#f
```

NOTE: It doesn't align with R7RS standard in certain cases like this (`gosh` gives `9` for the following code):
```
minsc.rs> (letrec ((x (* y 2)) (y 3)) (+ x y))
Eval Error: undefined variable: y
```
