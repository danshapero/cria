
open OUnit
open Expressions

let test_fixture = "TypeChecker" >:::
[
  "atoms" >::
    ( fun () ->
      assert_equal SymbolType (typeof (Atom (Symbol "reduce")) empty_context);
      assert_equal IntegerType (typeof (Atom (Integer 2)) empty_context);
      assert_equal RealType (typeof (Atom (Real 1.0)) empty_context);
      assert_equal BooleanType (typeof (Atom (Boolean true)) empty_context);
      assert_equal BooleanType (typeof (Atom (Boolean false)) empty_context);
      assert_equal CharacterType (typeof (Atom (Character 'a')) empty_context);
    );

  "functions" >::
    ( fun () ->
      assert_equal (FunctionType (IntegerType, IntegerType))
                   (typeof (Expression [(Atom (Symbol "lambda"));
                                        (Expression [(Atom (Symbol "x"));
                                                     (Atom (Symbol "int"))]);
                                        (Expression [(Atom (Symbol "+"));
                                                     (Atom (Symbol "x"));
                                                     (Atom (Integer 4))])])
                           empty_context);
    );

  "conditionals" >::
    ( fun () ->
      assert_equal CharacterType
                   (typeof (Expression [(Atom (Symbol "if"));
                                        (Atom (Boolean true));
                                        (Atom (Character 'x'));
                                        (Atom (Character 'y'));])
                           empty_context);

      assert_raises (TypeCheckFailure "Conditional branches are different types")
                    (fun () ->
                     typeof (Expression [(Atom (Symbol "if"));
                                         (Atom (Boolean true));
                                         (Atom (Character 'x'));
                                         (Atom (Integer 42));])
                            empty_context);

      assert_raises (TypeCheckFailure "Conditional not Boolean")
                    (fun () ->
                     typeof (Expression [(Atom (Symbol "if"));
                                         (Atom (Integer 4));
                                         (Atom (Character 'x'));
                                         (Atom (Character 'y'));])
                            empty_context);
    );

  "lets" >::
    ( fun () ->
      assert_equal IntegerType
                   (typeof (Expression [(Atom (Symbol "let"));
                                        (Expression [(Atom (Symbol "x"));
                                                     (Atom (Symbol "int"))]);
                                        (Expression [(Atom (Symbol "+"));
                                                     (Atom (Symbol "x"));
                                                     (Atom (Integer 1))]);])
                           empty_context);
    );

   "applications" >::
    ( fun () ->
      assert_equal IntegerType
                   (typeof (Expression [(Atom (Symbol "f"));
                                        (Atom (Symbol "x"))])
                           (add_context "f"
                                        (FunctionType (IntegerType, IntegerType))
                                        (add_context "x"
                                                     IntegerType
                                                     empty_context)))
    );
]

let _ = run_test_tt test_fixture
