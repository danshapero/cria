
open DataTypes
open Expressions
open Normalize
open OUnit

exception ParseFail;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  match e with
  | Some e -> e
  | None   -> raise ParseFail


let is_atomic e =
  match e with
  | Const _ -> true
  | Var _ -> true
  | _ -> false

let rec is_normalized e =
  match e with
  | Const _ -> true
  | Var _ -> true
  | App (f, args) -> (is_atomic f) && (List.for_all is_atomic args)
  | Abs (_, body) -> is_normalized body
  | Let (bindings, body) ->
    let rec normalized_bindings bindings =
      match bindings with
      | [] -> true
      | (_, e) :: bindings -> is_normalized e && normalized_bindings bindings
    in
    normalized_bindings bindings && is_normalized body
  | Fix f -> is_normalized f
  | Cond (cond, t, f) -> is_atomic cond && is_normalized t && is_normalized f
  | Def (_, e) -> is_normalized e

let test_fixture = "Normalizer" >:::
[
  "constants" >::
    ( fun () ->
      let constants = [Const (Int 27);
                       Const (Float 5.0);
                       Const (Bool true);
                       Const (Bool false)]
      in
      assert_equal constants (List.map normalize constants)
    );

  "variables" >::
    ( fun () ->
      let v = Var "x" in
      assert_equal v (normalize v)
    );

  "applications" >::
    ( fun () ->
      let e = parse "(f x y)" in
      assert_equal e (normalize e);
      let e = parse "(f (+ a x) y)" in
      match normalize e with
      | Let ((sym, e) :: [], body)
        -> assert_equal body (App(Var "f", [Var sym; Var "y"]))
      | Let ((_, _) :: bindings, _)
        -> assert_failure "Should only have one let-binding!"
      | _ -> assert_failure "No let-binding for nested application!"
    );

  "abstractions" >::
    ( fun () ->
      let e = parse "(lambda [x:int] (+ x 1))" in
      assert_equal e (normalize e)
    );

  "let" >::
  ( fun () ->
      let e = parse "(let [x 1] x)" in
      assert_equal e (normalize e);
      let e = parse "(let [z (let [y (* x x)] (- 1.0 y))] (/ 1.0 z))" in
      assert_equal e (normalize e)
  );

  "conditionals" >::
    ( fun () ->
      let e = parse "(if b 0 1)" in
      assert_equal e (normalize e)
    )
]

let _ = run_test_tt_main test_fixture

