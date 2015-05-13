{
  open Tokens

  exception Error of string
}


rule lex = parse
  | [' ' '\t' '\n']                { lex lexbuf }
  | "("                            { LPAREN }
  | ")"                            { RPAREN }
  | ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s { ID (s) }
  | ['0'-'9']+ as i                { INT (int_of_string i) }
  | ['0'-'9']+ "." ['0'-'9']* as x { REAL (float_of_string x) }
  | "true"                         { BOOL (true) }
  | "false"                        { BOOL (false) }
  | eof                            { EOF }