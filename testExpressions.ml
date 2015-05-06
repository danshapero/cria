
open OUnit
open Expressions

let test_fixture = "TypeChecker" >:::
[
  "atoms" >::
    ( fun () ->
      assert_equal SymbolType (typeof (Atom (Symbol "reduce")));
      assert_equal IntegerType (typeof (Atom (Integer 2)));
      assert_equal RealType (typeof (Atom (Real 1.0)));
      assert_equal BooleanType (typeof (Atom (Boolean true)));
      assert_equal BooleanType (typeof (Atom (Boolean false)));
      assert_equal CharacterType (typeof (Atom (Character 'a')));
    );
]

let _ = run_test_tt test_fixture
