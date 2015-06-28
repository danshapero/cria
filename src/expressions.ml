
type const =
  | Nil
  | Int of int
  | Float of float
  | Bool of bool

type t =
  | Const of const
  | Var of Var.t
  | App of t * t list
  | Abs of (Var.t * DataTypes.t) list * t
  | Let of (Var.t * t) list * t
  | Fix of t
  | Cond of (t * t * t)
  | Def of (Var.t * t)

