
open Core.Std
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let expr_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let () =
  let s = "(lambda (x:int) (+ x 1))" in
  let e = expr_of_string s in
  match e with
  | Some e -> Printf.printf "%s\n" (Expressions.string_of_expr e)
  | None   -> Printf.printf "\n"
