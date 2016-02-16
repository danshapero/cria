
(** Atomic expressions are guaranteed to:
    - terminate
    - cause no side effects
    - cause no control effects
    - never produce an error *)
type a =
  | Const of Expr.const
  | Var of Var.t
  | Abs of (Var.t * DataTypes.t) list * t

(** Complex expressions which can have control effects... *)
and c =
  | App of a * a list
  | Cond of a * a * a

(** must be let-bound. *)
and t =
  | Let of (Var.t * t) list * t
  | Complex of c
  | Atom of a
