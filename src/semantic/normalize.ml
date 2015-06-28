
let atomic = function
  | Expr.Const c -> true
  | Expr.Var x -> true
  | _ -> false

let rec normalize term =
  match term with
  | Expr.Const c -> term
  | Expr.Var x -> term
  | Expr.App (f, args)
    -> normalize_name f (fun f ->
        normalize_names args (fun args -> Expr.App (f, args)))
  | Expr.Abs (args, body) -> Expr.Abs (args, normalize body)
  | Expr.Let ([], body) -> normalize body
  | Expr.Let ((x, e) :: bindings, body) ->
    Expr.Let ([x, normalize e], normalize (Expr.Let(bindings, body)))
  | Expr.Fix f -> Expr.Fix (normalize f)
  | Expr.Cond (cond, t, f) -> normalize_name cond
                                (fun c ->
                                   Expr.Cond (c, normalize t, normalize f))
  | Expr.Def (var, e) -> Expr.Def (var, normalize e)
and normalize_name term k =
  if (atomic term) then
    k term
  else
    let v = Var.gen_var "g" in
    Expr.Let ([v, term], k (Expr.Var v))
and normalize_names terms k =
  match terms with
  | [] -> k []
  | term :: terms ->
    normalize_name term (fun term ->
                         normalize_names terms (fun terms ->
                                                k (term :: terms)))
