
This directory contains sources for the semantic analysis of the parsed source code.
The modules in this directory depend heavily on the `Expr` module in the `syntactic` directory.

The `DataTypes` module recursively defines the allowable types of expressions of the language.

The `TypeChecker` module defines a function which accepts an expression of the language and returns the type of that expression, if the expression is well-typed, and throws an error otherwise.
For example, the type checker enforces the rule that the guard of a conditional expression must be a boolean, or that the function to call in a call expression actually is a function.
Additionally, the type checker constructs *typing contexts*, which keep track of the types of values that have been bound to variables.
For example, if you were to define a function `factorial` to compute factorials, a typing context would keep track of the fact that `factorial` is bound to a function of type `int -> int`.

The `Normalize` module takes complex expressions and simplifies them to a form more amenable to compilation.
For example, a function call expression with complex arguments is transformed to a call expression with atomic arguments bound by a surrounding `let` statement, e.g.
```
(f (+ x y) (g z))
```
is transformed to
```
(let [g#1 (+ x y)]
  (let [g#2 (g z)]
    (f g#1 g#2)))
```
This enforces an evaluation order on sub-expressions.