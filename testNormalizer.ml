
open DataTypes
open Expressions
open Normalize

exception ParseFail;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  match e with
  | Some e -> e
  | None   -> raise ParseFail


let () =
  let e1 = (parse "(if b 0 1)") in
  let s1 = string_of_expr 0 (normalize e1) in
  print_endline s1;
