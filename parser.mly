
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> ID
%token COLON
%token LPAREN
%token RPAREN
%token EOF
%start <Expressions.expr option> prog

%%

prog:
  | EOF      { None }
  | e = expr { Some e }

expr:
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
