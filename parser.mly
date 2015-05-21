
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> ID
%token COLON
%token LAMBDA
%token LPAREN
%token RPAREN
%token EOF
%start <Expressions.expr option> prog

%%

prog:
  | EOF      { None }
  | e = expr { Some e }

expr:
  | LPAREN; LAMBDA; LPAREN; args = var_decl_list; RPAREN; body = expr; RPAREN
    { `Abstraction (args, body) }
  | LPAREN; e1 = expr; e2 = expr; RPAREN
    { `Application (e1, e2) }
  | i = INT
    { `Constant (`Int i) }
  | x = FLOAT
    { `Constant (`Float x) }
  | p = BOOL
    { `Constant (`Bool p) }
  | s = ID
    { `Variable s }
  ;

var_decl_list:
  var_decls = list(var_decl)
  { var_decls }
  ;

var_decl:
  x = ID; COLON; t = ID (* temporary while types are strings *)
  { (x, t) }
  ;
