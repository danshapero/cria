
open OUnit
open Expressions
open Lexer
open Lexing
open TypeChecker

exception ParseFail of string;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  match e with
  | Some e -> e
  | None   -> raise (ParseFail "Parsing expression failed!")

let test_fixture = "TypeChecker" >:::
[
  "constants" >::
    ( fun () ->
      assert_equal Int_t (typeof (parse "1") empty_context);
      assert_equal Float_t (typeof (parse "1.0") empty_context);
      assert_equal Bool_t (typeof (parse "true") empty_context);
      assert_equal Bool_t (typeof (parse "false") empty_context)
    );

  "abstractions" >::
    ( fun () ->
      assert_equal (Function_t ([Int_t], Int_t))
                   (typeof (parse "(lambda (x:int):int x)")
                           empty_context);
      assert_equal (Function_t ([Int_t; Int_t], Int_t))
                   (typeof (parse "(lambda (x:int y:int):int (+ x y))")
                           default_context)
    );

  "applications" >::
    ( fun () ->
      assert_equal Int_t
                   (typeof (parse "(+ 1 2)") default_context);
      assert_equal Bool_t
                   (typeof (parse "(> 4 2)") default_context);
      assert_equal Int_t
                   (typeof (parse "((lambda (x:int):int (+ x 1)) 1)")
                           default_context)
    );

  "let" >::
    ( fun () ->
      assert_equal Int_t
                   (typeof (parse "(let (x:int 1) (+ x 1))")
                           default_context);
      assert_equal (Function_t ([Int_t], Int_t))
                   (typeof
                      (parse "(let (x:int 42) (lambda (k:int):int (* k x)))")
                      default_context)
    );

  "conditionals" >::
    ( fun () ->
      assert_equal Int_t
                   (typeof (parse "(if true 1 2)") empty_context);
      assert_equal (Function_t ([Int_t], Int_t))
                   (typeof
                      (parse "(lambda (x:int):int (* (if (> x 0) 1 -1) x))")
                      default_context)
    );
]

let _ = run_test_tt_main test_fixture
