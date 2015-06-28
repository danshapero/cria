
open Core
open Expressions

let string_of_constant c =
  match c with
  | Nil -> "()"
  | Int i -> string_of_int i
  | Float x -> string_of_float x
  | Bool p -> string_of_bool p

let indent' i s =
  let lines = Core_string.split s '\n' in
  String.concat ("\n" ^ (String.make i ' ')) lines

let indent i s =
  (String.make i ' ') ^ (indent' i s)

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
        List.map (fun (x, t) -> x ^ ":" ^ (DataTypes.string_of_data_type t)) args
      in
      let args = String.concat " " string_of_args in
      k (string_of_term
           body
           (fun s -> "(lambda [" ^ args ^ "]\n" ^ (indent 2 s) ^ ")"))
    | Let (bindings, body) ->
      let bindings = string_of_bindings bindings in
      k (string_of_term
           body
           (fun s ->
              "(let [" ^ (indent' 6 bindings) ^ "]\n" ^ (indent 2 s) ^ ")"))
    | Fix f ->
      k (string_of_term f (fun s -> "(fix " ^ s ^ ")"))
    | Cond (cond, t, f) ->
      k (string_of_term
           cond
           (fun cond ->
              string_of_term
                t
                (fun t ->
                   string_of_term f
                     (fun f ->
                        "(if " ^ cond ^ " " ^ t ^ " " ^ f ^ ")"))))
    | Def (var, e) ->
      k (string_of_term e (fun s -> "(def " ^ var ^ " " ^ s ^ ")"))
  and string_of_binding (x, e) =
    let len = String.length x in
    string_of_term e (fun s -> x ^ " " ^ (indent' (len + 1) s))
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

