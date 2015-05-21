
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
    | `Application of expr * expr
    | `Abstraction of variable * data_type * expr
    | `Let of (variable * data_type * expr) list * expr
    | `Letrec of (variable * data_type * expr) list * expr
  ]


open Core.Std
let string_of_constant c =
  match c with
  | `Int i   -> string_of_int i
  | `Float x -> Float.to_string x
  | `Bool p  -> string_of_bool p


let rec string_of_expr expr =
  match expr with
    | `Constant c            -> string_of_constant c
    | `Variable v            -> v
    | `Application (e1, e2)  -> let s1 = string_of_expr e1 in
                                let s2 = string_of_expr e2 in
                                "(" ^ s1 ^ " " ^ s2 ^ ")"
    | `Abstraction (v, t, e) -> let s = string_of_expr e in
                                "(lambda (" ^ v ^ ":" ^ t ^ ") " ^ s ^ ")"
    | `Let (bindings, e)     -> "(let "
                                ^ ")" (* temporary *)
    | `Letrec (bindings, e)  -> "(let " 
                                ^ ")" (* temporary *)
