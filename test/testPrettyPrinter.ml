
open Expr
open Lexing
open Lexer
open PrettyPrinter

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
     "(lambda [x:int] (lambda [] x))";
     "(+ x 3)";
     "(lambda [x:int] (lambda [k:int] (+ x k)))";
     "(fix (lambda [f:(int int -> int)]
        (lambda [a:int b:int]
          (if (= b 0)
              a
              (f b (mod a b))))))";
     "(let [x 1] x)";
     "(let [x 1]
        (lambda [k:int]
          (+ x k)))";
     "(let [x (+ k r)
            y (* q p)]
        (gcd x y))";
     "((if (> x 0) + -) x)";
     "(f (+ a b) (* x y))";
     "((lambda [x:int] (+ x 1)) 2)"]
  in
  let parsed_expressions = List.map parse expressions in
  List.iter print_endline (List.map string_of_expr parsed_expressions);
