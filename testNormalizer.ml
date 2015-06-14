
open DataTypes
open Expressions
open Normalize
open OUnit

exception ParseFail;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  match e with
  | Some e -> e
  | None   -> raise ParseFail


let test_fixture = "Normalizer" >:::
[
  "applications" >::
    ( fun () ->
      let e = parse "(f x y)" in
      assert_equal e (normalize e);
      let e = parse "(f (+ a x) y)" in
      match normalize e with
      | Let ((sym, e) :: [], body)
        -> assert_equal body (App(Var "f", [Var sym; Var "y"]))
      | Let ((x, e) :: bindings, body)
        -> assert_failure "Should only have one let-binding!"
      | _ -> assert_failure "No let-binding for nested application!"
    )
]

let _ = run_test_tt_main test_fixture

