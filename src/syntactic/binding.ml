
open Expr

module VarSet = Set.Make(Var)

let rec occurs_free var expr =
  match expr with
  | Const _ -> false
  | Var x -> var = x
  | App(f, args) -> (occurs_free var f) || (List.exists (occurs_free var) args)
  | Abs(args, body) ->
    let arg_names = List.map fst args in
    not (List.mem var arg_names) && (occurs_free var body)
  | Let (bindings, body) ->
    let bound_vars = List.map fst bindings in
    not (List.mem var bound_vars) && (occurs_free var body)
  | Fix f -> occurs_free var f
  | Cond (cond, t, f) ->
    not ((occurs_free var cond) || (occurs_free var t) || (occurs_free var f))
  | Def (x, body) ->
    not (var = x) && (occurs_free var body)


let default_bindings =
  VarSet.of_list ["+"; "-"; "*"; "/"; "^"; "%"; "<"; ">"; ">="; "<="; "=";
                  "and"; "or"; "not"; "printi"]
