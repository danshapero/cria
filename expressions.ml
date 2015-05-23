
type variable = string

type constant = [
    | `Int of int
    | `Float of float
    | `Bool of bool
  ]


(*
type data_type = Bool
               | Char
               | Int
               | Float
               | Function of data_type * data_type
 *)
type data_type = string

(** Going to need symbol types at some point... *)
(** Also algebraic data types... *)
(** Also arrays... *)

type expr = [
    | `Constant of constant
    | `Variable of variable
    | `Application of expr * expr list
    | `Abstraction of (variable * data_type) list * expr
    | `Let of (variable * data_type * expr) list * expr
    | `Letrec of (variable * data_type * expr) list * expr
  ]


let string_of_constant c =
  match c with
  | `Int i   -> string_of_int i
  | `Float x -> string_of_float x
  | `Bool p  -> string_of_bool p


(* Temporarily while types are strings *)
let string_of_data_type t = t


let string_of_arg_decl (name, ty) =
  name ^ ":" ^ ty


let rec string_of_expr expr =
  match expr with
    | `Constant c            -> string_of_constant c
    | `Variable v            -> v
    | `Application (f, args) -> let sf = string_of_expr f in
                                let sa = List.map string_of_expr args in
                                "(" ^ sf ^ " " ^ (String.concat " " sa) ^ ")"
    | `Abstraction (args, e) -> let s = string_of_expr e in
                                let a = List.map string_of_arg_decl args in
                                "(lambda (" ^ (String.concat " " a) ^ ") " ^ s ^ ")"
    | `Let (bindings, e)     -> "(let "
                                ^ ")" (* temporary *)
    | `Letrec (bindings, e)  -> "(let " 
                                ^ ")" (* temporary *)
