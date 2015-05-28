
%{
open Expressions
%}

%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> ID
%token COLON
%token LAMBDA
%token LET
%token LETREC
%token IF
%token BOOL_T
%token INT_T
%token FLOAT_T
%token ARROW
%token LPAREN
%token RPAREN
%token EOF
%start <Expressions.expr option> prog

%%

prog:
  | EOF      { None }
  | e = expr { Some e }

data_type:
  | BOOL_T   { Bool_t }
  | INT_T    { Int_t }
  | FLOAT_T  { Float_t }
  | LPAREN; args = list(data_type); ARROW; ret = data_type; RPAREN;
    { Function_t (args, ret) }
  ;

expr:
  | LPAREN; e = sexpr; RPAREN  { e }
  | i = INT                    { Constant (Int i) }
  | x = FLOAT                  { Constant (Float x) }
  | p = BOOL                   { Constant (Bool p) }
  | s = ID                     { Variable s }
  ;

sexpr:
  | LAMBDA; LPAREN; args = var_decl_list; RPAREN; COLON; ret = data_type;
      body = expr;
    { Abstraction (args, ret, body) }
  | f = expr; args = list(expr)
    { Application (f, args) }
  | LET; LPAREN; bindings = var_binding_list; RPAREN; body = expr;
    { Let (bindings, body) }
  | LETREC; LPAREN; bindings = var_binding_list; RPAREN; body = expr;
    { Letrec (bindings, body) }
  | IF; cond = expr; t_branch = expr; f_branch = expr
    { Conditional (cond, t_branch, f_branch) }
  ;

var_decl_list:
  var_decls = list(var_decl)
  { var_decls }
  ;

var_decl:
  x = ID; COLON; t = data_type
  { (x, t) }
  ;

var_binding_list:
  var_bindings = list(var_binding)
  { var_bindings }
  ;

var_binding:
  x = ID; COLON; t = data_type; e = expr
  { (x, t, e) }
  ;