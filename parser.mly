
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> ID
%token COLON
%token LPAREN RPAREN
%token EOF
%start <Expressions.expr option> prog

%%

prog:
  | EOF      { None }
  | e = expr { Some e }

expr:
  | i = INT
    { `Constant (`Int i) }
  | x = FLOAT
    { `Constant (`Float x) }
  | p = BOOL
    { `Constant (`Bool p) }
  | s = ID
    { `Variable s }
  ;
