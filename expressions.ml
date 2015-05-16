
type variable = string

type data_type = Bool 
               | Char
               | Int
               | Float
               | Function of data_type * data_type

(** Going to need symbol types at some point... *)
(** Also algebraic data types... *)
(** Also arrays... *)

type expr = Constant of constant
          | Variable of variable
          | Application of expr * expr
          | Abstraction of variable * data_type * expr
          | Let of [variable * data_type * expr] * expr
          | Letrec of [variable * data_type * expr] * expr
