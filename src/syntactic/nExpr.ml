
type a =
  | Const of Expr.const
  | Var of Var.t
  | Abs of (Var.t * DataTypes.t) list * t
  (* Does Fix belong here? It doesn't induce any control flow; the application
  to another term does... *)
  | Fix of t

and c =
  | App of a * a list
  | Cond of a * t * t

and t =
  | Def of Var.t * t
  | Let of (Var.t * t) list * t
  | Complex of c
  | Atom of a

let atomic = function
  | Expr.Const _ -> true
  | Expr.Var _ -> true
  | _ -> false

let rec normalize term =
  match term with
  | Expr.Const c -> Atom (Const c)
  | Expr.Var x -> Atom (Var x)
  | Expr.App (f, args) ->
    normalize_name f (fun name ->
      normalize_names args (fun arg_names -> Complex(App(name, arg_names))))
  | Expr.Abs (args, body) -> Atom (Abs (args, normalize body))
  | Expr.Let ([], body) -> normalize body
  | Expr.Let ((x, e) :: bindings, body) ->
    Let ([x, normalize e], normalize (Expr.Let(bindings, body)))
  | Expr.Fix f -> Atom (Fix (normalize f))
  | Expr.Cond (cond, t, f) ->
    normalize_name cond
      (fun c ->
         Complex (Cond (c, normalize t, normalize f)))
  | Expr.Def (var, e) -> Def (var, normalize e)

and normalize_name (term:Expr.t) (k:a -> t) =
  let atom =
    match term with
    | Expr.Const c -> Some (Const c)
    | Expr.Var v -> Some (Var v)
    | _ -> None
  in
  match atom with
  | Some a -> k a
  | None ->
    let v = Var.gen_var "g" in
    Let ([v, normalize term], k (Var v))

and normalize_names terms (k:a list -> t) =
  match terms with
  | [] -> k []
  | term :: terms ->
    normalize_name term (fun name ->
        normalize_names terms (fun names ->
          k (name :: names)))
