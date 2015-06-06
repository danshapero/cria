
open OUnit
open Expressions
open Lexer
open Lexing
open TypeChecker
open DefaultTypeContext

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
                   (typeof (parse "(lambda [x:int] x)")
                           empty_context);
      assert_equal (Function_t ([Int_t; Int_t], Int_t))
                   (typeof (parse "(lambda [x:int y:int] (+ x y))")
                           default_context);
      assert_equal (Function_t ([Int_t; Int_t], Bool_t))
                   (typeof (parse "(lambda [x:int y:int] (= (% x y) 0))")
                           default_context)
    );

  "applications" >::
    ( fun () ->
      assert_equal Int_t
                   (typeof (parse "(+ 1 2)") default_context);
      assert_equal Bool_t
                   (typeof (parse "(> 4 2)") default_context);
      assert_equal Int_t
                   (typeof (parse "((lambda [x:int] (+ x 1)) 1)")
                           default_context)
    );

  "let" >::
    ( fun () ->
      assert_equal Int_t
                   (typeof (parse "(let [x 1] (+ x 1))")
                           default_context);
      assert_equal (Function_t ([Int_t], Int_t))
                   (typeof
                      (parse "(let [x 42] (lambda [k:int] (* k x)))")
                      default_context)
    );

  "conditionals" >::
    ( fun () ->
      assert_equal Int_t
                   (typeof (parse "(if true 1 2)") empty_context);
      assert_equal (Function_t ([Int_t], Int_t))
                   (typeof
                      (parse "(lambda [x:int] (* (if (> x 0) 1 -1) x))")
                      default_context)
    );

  "fix" >::
    ( fun () ->
      let factorial_code =
        "(fix (lambda [fact:(int int -> int)]
           (lambda [n:int f:int]
             (if (= n 0)
                 f
                 (fact (- n 1) (* n f))))))"
      in
      assert_equal (typeof (parse factorial_code) default_context)
                   (Function_t ([Int_t; Int_t], Int_t))
    );

  "defines" >::
    ( fun () ->
      assert_equal (Function_t ([Int_t; Int_t], Bool_t))
                   (typeof
                      (parse "(def divides? (lambda [x:int y:int] (= (% x y) 0)))")
                      default_context)
    )
]

let _ = run_test_tt_main test_fixture
