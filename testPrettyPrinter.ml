
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
  let es0 = string_of_expr 0 (parse "(if b 0 1)") in
  let es1 = string_of_expr 0 (parse "(f (+ x 1) (* y 3))") in
  let es2 = string_of_expr 0 (parse "(lambda [x:int] (+ x 1))") in
  let es3 = string_of_expr 0 (parse "(let [x 1] (+ x 3 9 (* x 12)))") in
  let es4 = string_of_expr 0 (parse "(let [x 63  y 42] (gcd x y))") in
  print_endline es0;
  print_endline es1;
  print_endline es2;
  print_endline es3;
  print_endline es4;
