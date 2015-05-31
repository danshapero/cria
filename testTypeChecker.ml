
open OUnit
open Expressions
open TypeChecker

let test_fixture = "TypeChecker" >:::
[
  "constants" >::
    ( fun () ->
      assert_equal Int_t (typeof (Constant (Int 1)) empty_context);
      assert_equal Float_t (typeof (Constant (Float 1.0)) empty_context);
      assert_equal Bool_t (typeof (Constant (Bool true)) empty_context);
      assert_equal Bool_t (typeof (Constant (Bool false)) empty_context)
    );

  "abstractions" >::
    ( fun () ->
      assert_equal (Function_t ([Int_t], Int_t))
                   (typeof (Abstraction ([("x", Int_t)],
                                         Int_t,
                                         Variable "x"))
                           empty_context);
      assert_equal (Function_t ([Int_t; Int_t], Int_t))
                   (typeof (Abstraction ([("x", Int_t);
                                          ("y", Int_t)],
                                         Int_t,
                                         Application (Variable "+",
                                                      [Variable "x";
                                                       Variable "y"])))
                            default_context)
    );

  "applications" >::
    ( fun () ->
      assert_equal Int_t
                   (typeof (Application (Variable "+",
                                         [Constant (Int 1);
                                          Constant (Int 2)]))
                   default_context)

    );

  "let" >::
    ( fun () ->
      assert_equal Int_t
                   (typeof (Let ([("x", Int_t, Constant (Int 1))],
                                 (Application (Variable "+",
                                               [Variable "x";
                                                Constant (Int 1)]))))
                           default_context)

    );
]

let _ = run_test_tt_main test_fixture
