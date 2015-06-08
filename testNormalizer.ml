
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
  let expressions = ["(if b 0 1)";
                     "(* (if (> x 0) 1 -1) x)";
                     "(lambda [x:int y:int] (= (% x y) 0))";
                     "(lambda [p:int]
                        (lambda [k:int] (= (% k p) 0)))"]
  in
  let parsed_expressions = List.map parse expressions in
  let normalized_expressions = List.map normalize parsed_expressions in
  let expression_strings = List.map (string_of_expr 0) normalized_expressions in
  List.iter print_endline expression_strings;