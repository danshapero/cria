
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> ID
%token COLON
%token LAMBDA
%token LET
%token IF
%token LPAREN
%token RPAREN
%token EOF
%start <Expressions.expr option> prog

%%

prog:
  | EOF      { None }
  | e = expr { Some e }

expr:
  | LPAREN; e = sexpr; RPAREN  { e }
  | i = INT                    { `Constant (`Int i) }
  | x = FLOAT                  { `Constant (`Float x) }
  | p = BOOL                   { `Constant (`Bool p) }
  | s = ID                     { `Variable s }
  ;

sexpr:
  | LAMBDA; LPAREN; args = var_decl_list; RPAREN; body = expr;
    { `Abstraction (args, body) }
  | f = expr; args = list(expr)
    { `Application (f, args) }
  | LET; LPAREN; bindings = var_binding_list; RPAREN; body = expr;
    { `Let (bindings, body) }
  | IF; cond = expr; t_branch = expr; f_branch = expr
    { `If (cond, t_branch, f_branch) }
  ;

var_decl_list:
  var_decls = list(var_decl)
  { var_decls }
  ;

var_decl:
  x = ID; COLON; t = ID (* temporary while types are strings *)
  { (x, t) }
  ;

var_binding_list:
  var_bindings = list(var_binding)
  { var_bindings }
  ;

var_binding:
  x = ID; COLON; t = ID; e = expr
  { (x, t, e) }
  ;