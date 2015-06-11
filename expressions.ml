
open DataTypes

type variable = string

type constant =
  | Nil
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
  | Nil -> "()"
  | Int i -> string_of_int i
  | Float x -> string_of_float x
  | Bool p -> string_of_bool p

let indent i = String.make i ' '

exception PrettyPrintFail;;

let string_of_expr expr =
  let rec string_of_expr expr (k:string->string) =
    match expr with
    | Const c -> (k (string_of_constant c))
    | Var v -> (k v)
    | App (f, args) -> raise PrettyPrintFail
    | Abs (args, body) -> raise PrettyPrintFail
    | Let (bindings, body) -> raise PrettyPrintFail
    | Fix f -> raise PrettyPrintFail
    | Cond (cond, t, f) -> raise PrettyPrintFail
    | Def (var, e) ->
       (k (string_of_expr e (fun s -> "(def " ^ var ^ " " ^ s ^ ")")))
  in
  string_of_expr expr (fun s -> s)
