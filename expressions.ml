
type variable = string

type constant =
    | Int of int
    | Float of float
    | Bool of bool


type data_type = Bool_t
               | Int_t
               | Float_t
               | Function_t of (data_type list) * data_type

type expr =
    | Const of constant
    | Var of variable
    | App of expr * expr list
    | Abs of (variable * data_type) list * data_type * expr
    | Let of (variable * data_type * expr) list * expr
    | Letrec of (variable * data_type * expr) list * expr
    | Cond of (expr * expr * expr)


let string_of_constant c =
  match c with
  | Int i   -> string_of_int i
  | Float x -> string_of_float x
  | Bool p  -> string_of_bool p


let rec string_of_data_type t =
  match t with
    | Bool_t                 -> "bool"
    | Int_t                  -> "int"
    | Float_t                -> "float"
    | Function_t (args, ret) ->
       let args_string =
         String.concat " " (List.map string_of_data_type args)
       and ret_string =
         string_of_data_type ret
       in
       "(-> " ^ args_string ^ " " ^ ret_string ^ ")"


let rec string_of_expr expr =
  let string_of_binding (name, ty, expr) =
    name ^ ":" ^ (string_of_data_type ty) ^ " " ^ (string_of_expr expr)
  in
  let string_of_arg_decl (name, ty) =
    name ^ ":" ^ (string_of_data_type ty)
  in
  match expr with
    | Const c ->
       string_of_constant c
    | Var v ->
       v
    | App (f, args) ->
       let func = string_of_expr f
       and args = List.map string_of_expr args in
       "(" ^ func ^ " " ^ (String.concat " " args) ^ ")"
    | Abs (args, ret, body) ->
       let args = List.map string_of_arg_decl args
       and ret  = string_of_data_type ret
       and body = string_of_expr body in
       "(lambda (" ^ (String.concat " " args) ^ "):" ^ ret ^ " " ^ body ^ ")"
    | Let (bindings, body) ->
       let bindings = List.map string_of_binding bindings
       and body     = string_of_expr body in
       "(let (" ^ (String.concat " " bindings) ^ ") " ^ body ^ ")"
    | Letrec (bindings, body) ->
       let bindings = List.map string_of_binding bindings
       and body     = string_of_expr body in
       "(letrec (" ^ (String.concat " " bindings) ^ ") " ^ body ^ ")"
    | Cond (cond, t, f) ->
       "(if " ^ (string_of_expr cond) ^ " "
       ^ (string_of_expr t) ^ " "
       ^ (string_of_expr f) ^ ")"
