
open Core
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

exception PrettyPrintFail;;

let indent_after_first_line i s =
  let lines = Core_string.split s '\n' in
  String.concat ("\n" ^ (String.make i ' ')) lines

let indent i s =
  (String.make i ' ') ^ (indent_after_first_line i s)

let multiline s =
  String.contains "\n" s

let rec string_of_expr expr =
  let rec string_of_term expr (k:string->string) =
    match expr with
    | Const c -> k (string_of_constant c)
    | Var v -> k v
    | App (f, args) ->
      k (string_of_term
           f
           (fun s ->
              string_of_terms
                args
                (fun args -> "(" ^ s ^ " " ^ (String.concat " " args) ^ ")")))
    | Abs (args, body) ->
      let string_of_args =
        List.map (fun (x, t) -> x ^ ":" ^ (string_of_data_type t)) args
      in
      let args = String.concat " " string_of_args in
      k (string_of_term
           body
           (fun s -> "(lambda [" ^ args ^ "]\n" ^ (indent 2 s) ^ ")"))
    | Let (bindings, body) ->
      k (string_of_term
           body
           (fun s -> "(let [" ^ (indent_after_first_line 6 (string_of_bindings bindings)) ^ "]\n" ^ (indent 2 s) ^ ")"))
    | Fix f ->
      k (string_of_term f (fun s -> "(fix " ^ s ^ ")"))
    | Cond (cond, t, f) ->
      string_of_term
        cond
        (fun cond ->
           string_of_term
             t
             (fun t ->
                string_of_term f
                  (fun f ->
                     "(if " ^ cond ^ " " ^ t ^ " " ^ f ^ ")")))
    | Def (var, e) ->
      k (string_of_term e (fun s -> "(def " ^ var ^ " " ^ s ^ ")"))
  and string_of_binding (x, e) =
    x ^ " " ^ string_of_expr e
  and string_of_bindings bindings =
    String.concat "\n" (List.map string_of_binding bindings)
  and string_of_terms exprs (k:(string list)->string) =
    match exprs with
    | [] -> k []
    | term :: terms ->
      string_of_term
        term
        (fun s -> string_of_terms terms (fun terms -> k (s :: terms)))
  in
  string_of_term expr (fun s -> s)
