
open Binding
open OUnit
open Expr
open Lexer
open Lexing

exception ParseFail of string;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  match e with
    | Some e -> e
    | None   -> raise (ParseFail "Parsing expression failed!")


let test_fixture =
  "Free variables" >:::
  [
    "constants" >::
    ( fun () ->
        let exprs = List.map parse ["1"; "1.0"; "true"; "false"] in
        assert_equal false (List.exists (occurs_free "x") exprs)
    );

    "variables" >::
    (
      fun () ->
        assert_equal true (occurs_free "x" (Var "x"));
        assert_equal false (occurs_free "x" (Var "y"))
    );

    "abstractions" >::
    (
      fun () ->
        assert_equal false (occurs_free "x" (parse "(lambda [x:int] (+ x 1))"));
        assert_equal true (occurs_free "y" (parse "(lambda [x:int] (+ x y))"))
    )
  ]

let _ =
  run_test_tt_main test_fixture
