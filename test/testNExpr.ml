
open NExpr

exception ParseFail;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  match e with
  | Some e -> e
  | None -> raise ParseFail


let _ =
  let e = parse "(lambda [k:int] (lambda [x:int] (= 0 (/ x k))))" in
  normalize e
