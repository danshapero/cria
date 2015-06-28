
This directory contains sources for the syntactic analysis of source code into an AST suitable for analysis by the rest of the compiler.
For the most part, the source files in this directory depend only on themselves, with one exception: function definitions required knowledge of the types of the arguments.
The definition of data types in the language is in the `semantic` directory in the module `DataTypes`.

The module `Var` defines a type for variables in the source language, which are represented by their name, and a function to generate fresh identifiers.
Generating fresh identifiers is used in later phases of the compiler, chiefly the normalizer, which simplifies complex expressions.

The module `Expr` defines the syntax of the language.
For example, an function call expression consists of one expression (the function to call) and a list of arguments.
A conditional expression consists of a guard expression, a true branch expression and false branch expression.

The `lexer.mll` and `parser.mly` files are, respectively, input files for lexer and parser generators, which will automatically construct a lexer and parser for our source language from a specification of its syntax.
