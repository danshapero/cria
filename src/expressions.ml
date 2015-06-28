
type var = string

type const =
  | Nil
  | Int of int
  | Float of float
  | Bool of bool

type t =
  | Const of const
  | Var of var
  | App of t * t list
  | Abs of (var * DataTypes.t) list * t
  | Let of (var * t) list * t
  | Fix of t
  | Cond of (t * t * t)
  | Def of (var * t)

