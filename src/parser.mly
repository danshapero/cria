
%{
open DataTypes
open Expressions
%}

%token NIL
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <Var.t> ID
%token COLON
%token DEF
%token LAMBDA
%token LET
%token FIX
%token IF
%token NIL_T
%token BOOL_T
%token INT_T
%token FLOAT_T
%token ARROW
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token EOF
%start <Expressions.t option> prog

%%

prog:
  | EOF      { None }
  | e = expr { Some e }

data_type:
  | BOOL_T   { Bool_t }
  | INT_T    { Int_t }
  | FLOAT_T  { Float_t }
  | NIL_T    { Nil_t }
  | LPAREN; args = list(data_type); ARROW; ret = data_type; RPAREN;
    { Function_t (args, ret) }
  ;

expr:
  | LPAREN; RPAREN             { Const Nil }
  | LPAREN; e = sexpr; RPAREN  { e }
  | i = INT                    { Const (Int i) }
  | x = FLOAT                  { Const (Float x) }
  | p = BOOL                   { Const (Bool p) }
  | _ = NIL                    { Const Nil }
  | s = ID                     { Var s }
  ;

sexpr:
  | DEF; var = ID; e = expr;
    { Def (var, e) }
  | LAMBDA; LBRACK; args = var_decl_list; RBRACK; body = expr;
    { Abs (args, body) }
  | f = expr; args = list(expr)
    { App (f, args) }
  | LET; LBRACK; bindings = var_binding_list; RBRACK; body = expr;
    { Let (bindings, body) }
  | FIX; f = expr;
    { Fix f }
  | IF; cond = expr; t_branch = expr; f_branch = expr
    { Cond (cond, t_branch, f_branch) }
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
  x = ID; e = expr
  { (x, e) }
  ;