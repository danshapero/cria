
open OUnit
open Expressions
open Lexer
open Lexing

exception ParseFail of string;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  match e with
    | Some e -> e
    | None   -> raise (ParseFail "Parsing expression failed!")

let test_fixture = "Expression parser" >:::
[
  "atoms" >::
    ( fun () -> assert_equal (parse "1")
                             (Constant (Int 1));
                assert_equal (parse "1.0")
                             (Constant (Float 1.0));
                assert_equal (parse "4.0e9")
                             (Constant (Float 4.0e9));
                assert_equal (parse "4.0E9")
                             (Constant (Float 4.0e9));
                assert_equal (parse "true")
                             (Constant (Bool true));
                assert_equal (parse "false")
                             (Constant (Bool false))
    );

  "variables" >::
    (
      fun () -> assert_equal (parse "x")
                             (Variable "x");
                assert_equal (parse "+")
                             (Variable "+");
                assert_equal (parse "yes?")
                             (Variable "yes?")
    );

  "applications" >::
    (
      ( fun () ->
        assert_equal (parse "(f 0)")
                     (Application (Variable "f",
                                   [Constant (Int 0)]));
        assert_equal (parse "(g x y 0)")
                     (Application (Variable "g",
                                   [Variable "x";
                                    Variable "y";
                                    Constant (Int 0)]));
      )
    );

  "abstractions" >::
    (
      ( fun () ->
        assert_equal (parse "(lambda [x:int]:int (+ x 1))")
                     (Abstraction ([("x", Int_t)],
                                   Int_t,
                                   (Application (Variable "+",
                                                 [Variable "x";
                                                  Constant (Int 1)]))))
      )
    );

  "conditionals" >::
    (
      ( fun () ->
        assert_equal (parse "(if b 0 1)")
                     (Conditional (Variable "b",
                                   Constant (Int 0),
                                   Constant (Int 1)))
      )
    );

]

let _ =
  run_test_tt_main test_fixture
