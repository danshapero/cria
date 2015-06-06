
open DataTypes

type variable = string

type constant =
    | Int of int
    | Float of float
    | Bool of bool

type expr =
    | Const of constant
    | Var of variable
    | App of expr * expr list
    | Abs of (variable * data_type) list * expr
    | Let of (variable * expr) list * expr
    | Fix of expr
    | Cond of (expr * expr * expr)
    | Def of (variable * expr)


let string_of_constant c =
  match c with
  | Int i   -> string_of_int i
  | Float x -> string_of_float x
  | Bool p  -> string_of_bool p

let indent i = String.make i ' '

let rec string_of_expr level expr =
  let string_of_binding (name, expr) =
    name ^ " " ^ (string_of_expr 1 expr)
  and string_of_arg_decl (name, t) =
    name ^ ":" ^ (string_of_data_type t)
  in
  (indent level) ^
  match expr with
  | Const c ->
    string_of_constant c
  | Var v ->
    v
  | App (f, args) ->
    let f = string_of_expr 0 f in
    let args = List.map (string_of_expr 0) args in
    "(" ^ f ^ " " ^ (String.concat " " args) ^ ")"
  | Abs (args, body) ->
    let args = List.map string_of_arg_decl args in
    let decl = "(lambda [" ^ (String.concat " " args) ^ "]" in
    let level = level + 2 in
    let body = string_of_expr level body in
    decl ^ "\n" ^ body ^ ")"
  | Let (bindings, body) ->
    let bindings = List.map string_of_binding bindings
    and binding_level = level + (String.length "(let [") in
    let separator = "\n" ^ (indent binding_level) in
    let decl = "(let [" ^ (String.concat separator bindings) ^ "]" in
    decl ^ "\n" ^ (string_of_expr (level + 2) body) ^ ")"
  | Fix f ->
     "(fix" ^ string_of_expr 1 f ^ ")"
  | Cond (cond, t, f) ->
    let level = level + 4 in
    "(if " ^ (string_of_expr 0 cond) ^ "\n"
    ^ (string_of_expr level t) ^ "\n"
    ^ (string_of_expr level f) ^ ")"
  | Def (var, e) ->
    "(def " ^ var ^ "\n" ^ (string_of_expr (level + 2) e)
