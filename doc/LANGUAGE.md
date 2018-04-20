# Language

What language does this "scheme interpreter" (attempt to) implement?


# Data types

We currently have the following data types

* booleans: `#t`, `#f`
* symbols: `foo`, `bar`, `quux`
* numbers: `33`, `-524` (only integers)
* characters: `#\x`, `#\(`
* strings: "hello world!"
* nil: `()`
* cons: `(_ . _)`
* the EOF object (for detecting the end of a file)
* closures (constructed using lambda)


# Syntax

Looking over the data types already provides a rough overview of the syntax.

`;` is the line comment character.
We support quasiquote quote and unquote shorthand.
We support dotted pairs.

NOTE: long character literals like `#\newline` are yet to be implemented.


# Special forms

With reference to the evaluator function, the special forms implemented are:

* self evaluating forms (datums)
* `(quote <data>)`
* `(begin <exp> ...)`
* `(if <exp> <exp> <exp>)`
* `(lambda <arg> <exp> ...)`
* `(f x y z ...)`

and at the top level

* `(define <name> <body> ...)`
* `(define (<name> <args> ...) <body> ...)`


# Functions

The builtin functions provided are

```
display newline eq?
cons car cdr set-car! set-cdr!
+ - * quotient remainder
eof-object? symbol? number? char? string? null? pair? boolean? procedure?
< > <= >=
```

