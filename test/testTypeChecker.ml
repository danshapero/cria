
open OUnit
open DataTypes
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

let test_type_check = "TypeChecker" >:::
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
                      (parse "(def divides? (lambda [x:int y:int]
                                (= (% x y) 0)))")
                      default_context)
    )
]


let test_type_check_failures = "TypeCheckFail" >:::
[
  "conditionals" >::
    ( fun () ->
      assert_raises (TypeCheckFailure "Condition not a boolean!")
                    (fun () ->
                     typeof (parse "(if 0 2.71 3.14)") default_context);
      assert_raises
        (TypeCheckFailure "Types of conditional branches don't match!")
        (fun () -> typeof (parse "(if (> 1 0) true 3.14)") default_context)
    )
]


let test_type_check_program = "TypeCheckProgram" >:::
[
  "global defines" >::
  ( fun () ->
      let code =
        ["(def gcd
           (fix (lambda [f:(int int -> int)]
                   (lambda [a:int b:int]
                      (if (= b 0)
                          a
                          (f b (% a b)))))))";
         "(gcd 42 24)"]
      in
      let context = typecheck (List.map parse code) default_context in
      assert_equal (Function_t ([Int_t; Int_t], Int_t))
                   (StringMap.find "gcd" context)
  )
]

let _ = run_test_tt_main test_type_check
let _ = run_test_tt_main test_type_check_failures
let _ = run_test_tt_main test_type_check_program
