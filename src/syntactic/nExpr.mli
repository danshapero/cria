
type a =
  | Const of Expr.const
  | Var of Var.t
  | Abs of (Var.t * DataTypes.t) list * t
  | Fix of t

and c =
  | App of a * a list
  | Cond of a * a * a

and t =
  | Def of Var.t * t
  | Let of (Var.t * t) list * t
  | Complex of c
  | Atom of a

val normalize : Expr.t -> t
