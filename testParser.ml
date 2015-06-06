
open OUnit
open DataTypes
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
                             (Const (Int 1));
                assert_equal (parse "1.0")
                             (Const (Float 1.0));
                assert_equal (parse "4.0e9")
                             (Const (Float 4.0e9));
                assert_equal (parse "4.0E9")
                             (Const (Float 4.0e9));
                assert_equal (parse "true")
                             (Const (Bool true));
                assert_equal (parse "false")
                             (Const (Bool false))
    );

  "variables" >::
    (
      fun () -> assert_equal (parse "x") (Var "x");
                assert_equal (parse "+") (Var "+");
                assert_equal (parse "yes?") (Var "yes?")
    );

  "applications" >::
    (
      ( fun () ->
        assert_equal (parse "(f 0)")
                     (App (Var "f", [Const (Int 0)]));
        assert_equal (parse "(g x y 0)")
                      (App (Var "g",
                            [Var "x"; Var "y"; Const (Int 0)]));
      )
    );

  "abstractions" >::
    (
      ( fun () ->
        assert_equal (parse "(lambda [x:int] (+ x 1))")
                     (Abs ([("x", Int_t)],
                           (App (Var "+",
                                 [Var "x"; Const (Int 1)]))));
        assert_raises Parser.Error
                      (fun () -> parse "(lambda [x]:int (+ x 1))");
        assert_raises Parser.Error
                      (fun () -> parse "(lambda [x:int])")
      )
    );

  "conditionals" >::
    (
      ( fun () ->
        assert_equal (parse "(if b 0 1)")
                     (Cond (Var "b", Const (Int 0), Const (Int 1)));
        assert_raises Parser.Error
                      (fun () -> parse "(if b 0)");
      )
    );

  "let" >::
    ( fun () ->
      assert_equal (parse "(let [x 1] (+ x 2))")
                   (Let (["x", Const (Int 1)],
                         App (Var "+",
                              [Var "x"; Const (Int 2)])));
      assert_equal (parse "(let [x 613  y 42] (lcm x y))")
                   (Let (["x", Const (Int 613);
                          "y", Const (Int 42)],
                         App (Var "lcm",
                              [Var "x"; Var "y"])));

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
      assert_equal (parse factorial_code)
                   (Fix (Abs (["fact", Function_t ([Int_t; Int_t], Int_t)],
                              (Abs (["n", Int_t; "f", Int_t],
                                    Cond (App (Var "=",
                                               [Var "n";
                                                Const (Int 0)]),
                                          Var "f",
                                          App (Var "fact",
                                               [App (Var "-",
                                                     [Var "n";
                                                      Const (Int 1)]);
                                                App (Var "*",
                                                     [Var "n";
                                                      Var "f"])])))))))
    );

  "defines" >::
    ( fun () ->
      assert_equal (parse "(def x 1)")
                   (Def ("x", Const (Int 1)));
      assert_equal (parse "(def inc (lambda [x:int] (+ x 1)))")
                   (Def ("inc", Abs (["x", Int_t],
                                     App (Var "+",
                                          [Var "x"; Const (Int 1)]))))
    )

]

let _ =
  run_test_tt_main test_fixture
