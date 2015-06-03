
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
                                                  Constant (Int 1)]))));
        assert_raises Parser.Error
                      (fun () -> parse "(lambda [x:int] (+ x 1))");
        assert_raises Parser.Error
                      (fun () -> parse "(lambda [x]:int (+ x 1))");
        assert_raises Parser.Error
                      (fun () -> parse "(lambda [x:int]:int)")
      )
    );

  "conditionals" >::
    (
      ( fun () ->
        assert_equal (parse "(if b 0 1)")
                     (Conditional (Variable "b",
                                   Constant (Int 0),
                                   Constant (Int 1)));
        assert_raises Parser.Error
                      (fun () -> parse "(if b 0)");
      )
    );

  "let" >::
    ( fun () ->
      assert_equal (parse "(let [x:int 1] (+ x 2))")
                   (Let (["x", Int_t, Constant (Int 1)],
                         Application (Variable "+",
                                      [Variable "x"; Constant (Int 2)])));
      assert_raises Parser.Error
                    (fun () -> parse "(let [x 1] (+ x 2))");
      assert_equal (parse "(let [x:int 613  y:int 42] (lcm x y))")
                   (Let (["x", Int_t, Constant (Int 613);
                          "y", Int_t, Constant (Int 42)],
                         Application (Variable "lcm",
                                      [Variable "x"; Variable "y"])));
      let factorial_code =
        "(letrec [fact:(int int -> int)
                    (lambda [n:int f:int]:int
                            (if (= n 0)
                                f
                                (fact (- n 1) (* n f))))]
           (fact 5 1))"
      in
      assert_equal (parse factorial_code)
                   (Letrec (["fact",
                             Function_t ([Int_t; Int_t], Int_t),
                             Abstraction
                               (["n", Int_t;
                                 "f", Int_t],
                                Int_t,
                                Conditional
                                  (Application (Variable "=",
                                                [Variable "n";
                                                 Constant (Int 0)]),
                                   Variable "f",
                                   Application (Variable "fact",
                                                [Application (Variable "-",
                                                              [Variable "n";
                                                               Constant (Int 1)]);
                                                 Application (Variable "*",
                                                              [Variable "n";
                                                               Variable "f"])])))],
                            Application (Variable "fact",
                                         [Constant (Int 5);
                                          Constant (Int 1)])))
    )

]

let _ =
  run_test_tt_main test_fixture
