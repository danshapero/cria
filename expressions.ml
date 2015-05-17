
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
    | `Let of (variable * data_type * expr list) * expr
    | `Letrec of (variable * data_type * expr list) * expr
  ]


open Core.Std
let output_constant c =
  match c with
  | `Int i   -> printf "%d" i
  | `Float x -> printf "%g" x
  | `Bool p  -> printf "%B" p


let rec output_expr outc expr =
  match expr with
    | `Constant c            -> output_constant c
    | `Variable v            -> printf "%s" v
    | `Application (e1, e2)  -> output_string outc "(";
                                output_expr outc e1;
                                output_string outc " ";
                                output_expr outc e2;
                                output_string outc ")"
    | `Abstraction (v, _, e) -> output_string outc "(lambda (";
                                printf "%s" v;
                                output_string outc ") ";
                                output_expr outc e;
                                output_string outc ") ";
    | `Let (bindings, e)     -> output_string outc "(";
                                output_string outc "let ";
                                output_string outc ") " (* temporary *)
    | `Letrec (bindings, e)  -> output_string outc "(";
                                output_string outc "let ";
                                output_string outc ") " (* temporary *)
