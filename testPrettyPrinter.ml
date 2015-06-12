
open Expressions
open Lexing
open Lexer


exception ParseFail;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  match e with
  | Some e -> e
  | None   -> raise ParseFail

let () =
  let expressions =
    ["1";
     "v";
     "(def x 3)";
     "(lambda [x:int y:int] x)";
     "(lambda [x:int] (lambda [] x))"]
  in
  let parsed_expressions = List.map parse expressions in
  let expression_strings = List.map string_of_expr parsed_expressions in
  List.iter print_endline expression_strings;
