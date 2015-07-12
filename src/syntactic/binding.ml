
open Expr

module VarSet = Set.Make(Var)
module VarMap = Map.Make(Var)

let rec occurs_free var expr =
  match expr with
  | Const _ -> false
  | Var x -> var = x
  | App(f, args) -> (occurs_free var f) || (List.exists (occurs_free var) args)
  | Abs(args, body) ->
    let arg_names = List.map fst args in
    not (List.mem var arg_names) && (occurs_free var body)
  | Let(bindings, body) ->
    let bound_vars = List.map fst bindings in
    not (List.mem var bound_vars) && (occurs_free var body)
  | Fix f -> occurs_free var f
  | Cond(cond, t, f) ->
    not ((occurs_free var cond) || (occurs_free var t) || (occurs_free var f))
  | Def(x, body) ->
    not (var = x) && (occurs_free var body)


let rename_var new_var_names var =
  if VarMap.mem var new_var_names then
    VarMap.find var new_var_names
  else
    var


let rec rename new_var_names expr =
  match expr with
  | Const _ -> expr
  | Var x -> Var (rename_var new_var_names x)
  | App(f, args) ->
    let f = rename new_var_names f
    and args = List.map (rename new_var_names) args in
    App(f, args)
  | Abs(args, body) ->
    rename_abs new_var_names args body
  | Let(bindings, body) ->
    rename_let new_var_names bindings body
  | Fix f -> Fix (rename new_var_names f)
  | Cond (cond, t, f) ->
    let cond = rename new_var_names cond
    and t = rename new_var_names t
    and f = rename new_var_names f in
    Cond(cond, t, f)
  | Def(x, body) ->
    let new_x = rename_var new_var_names x in
    let new_var_names = VarMap.add x new_x new_var_names in
    Def(new_x, rename new_var_names body)

and rename_abs new_var_names args body =
  let rec rename_args new_var_names args =
    match args with
    | [] -> new_var_names
    | (x, _) :: args ->
      let new_x = rename_var new_var_names x in
      rename_args (VarMap.add x new_x new_var_names) args
  in
  let new_var_names = rename_args new_var_names args in
  let args = List.map (fun (x, t) -> (VarMap.find x new_var_names, t)) args in
  Abs(args, rename new_var_names body)

and rename_let new_var_names bindings body =
  let rec rename_bindings new_var_names bindings =
    match bindings with
    | [] -> new_var_names
    | (x, _) :: bindings ->
      let new_x = (rename_var new_var_names x) in
      rename_bindings (VarMap.add x new_x new_var_names) bindings
  in
  let new_var_names = rename_bindings new_var_names bindings in
  let bindings =
    List.map (fun (x, e) -> (VarMap.find x new_var_names, rename new_var_names e)) bindings
  in
  Let(bindings, rename new_var_names body)


let default_bindings =
  VarSet.of_list ["+"; "-"; "*"; "/"; "^"; "%"; "<"; ">"; ">="; "<="; "=";
                  "and"; "or"; "not"; "printi"]
