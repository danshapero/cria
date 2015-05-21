
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

let string_of_args_list args =
  let rec f args s =
    match args with
    | []             -> s
    | (x, t) :: []   -> s ^ x ^ ":" ^ (string_of_data_type t)
    | (x, t) :: args -> f args (s ^ x ^ ":" ^ (string_of_data_type t) ^ " ")
  in
  f args ""


let rec string_of_expr expr =
  match expr with
    | `Constant c            -> string_of_constant c
    | `Variable v            -> v
    | `Application (e1, e2)  -> let s1 = string_of_expr e1 in
                                let s2 = string_of_expr e2 in
                                "(" ^ s1 ^ " " ^ s2 ^ ")"
    | `Abstraction (args, e) -> let s = string_of_expr e in
                                let a = string_of_args_list args in
                                "(lambda (" ^ a ^ ") " ^ s ^ ")"
    | `Let (bindings, e)     -> "(let "
                                ^ ")" (* temporary *)
    | `Letrec (bindings, e)  -> "(let " 
                                ^ ")" (* temporary *)
