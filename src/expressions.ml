
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

