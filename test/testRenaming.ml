
open Binding
open OUnit
open Lexer
open Lexing

exception ParseFail of string;;

let parse s =
  let lexbuf = Lexing.from_string s in
  let e = Parser.prog Lexer.read lexbuf in
  match e with
  | Some e -> e
  | None   -> raise (ParseFail "Parsing expression failed!")

let rec bound var expr =
  match expr with
  | Expr.Const _ -> false
  | Expr.Var _ -> false
  | Expr.App(f, args) -> (bound var f) || (List.exists (bound var) args)
  | Expr.Abs(args, body) ->
    let arg_names = List.map fst args in
    (List.mem var arg_names) || (bound var body)
  | Expr.Let(bindings, body) ->
    let rec check_bindings = function
      | [] -> false
      | (x, e) :: bindings ->
        if (var = x) || (bound var e) then
          true
        else
          check_bindings bindings
    in
    (check_bindings bindings) || (bound var body)
  | Expr.Fix f -> bound var f
  | Expr.Cond(cond, t, f) -> (bound var cond) || (bound var t) || (bound var f)
  | Expr.Def(x, body) -> (var = x) || (bound var body)


let rec multiple_binding = function
  | Expr.Const _ -> false
  | Expr.Var _ -> false
  | Expr.App(f, args) ->
    (multiple_binding f) || (List.exists multiple_binding args)
  | Expr.Abs(args, body) ->
    let arg_names = List.map fst args in
    if (List.exists (fun v -> bound v body) arg_names) then
      true
    else
      multiple_binding body
  | Expr.Let(bindings, body) ->
    let rec check_bindings = function
      | [] -> multiple_binding body
      | (x, e) :: bindings ->
        if (bound x e) || (bound x body) then
          true
        else
          check_bindings bindings
    in
    (check_bindings bindings) || multiple_binding body
  | Expr.Fix f -> multiple_binding f
  | Expr.Cond(cond, t, f) ->
    (multiple_binding cond) || (multiple_binding t) || (multiple_binding f)
  | Expr.Def(x, body) ->
    (bound x body) || (multiple_binding body)

let test_fixture =
  "Let bindings" >:::
  [
    "no renaming" >::
    ( fun () ->
        let expr = parse "(let [x 1] (+ x 2))" in
        assert_equal true (bound "x" expr)
    );
  ]


let _ =
  run_test_tt_main test_fixture
