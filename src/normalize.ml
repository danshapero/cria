
open Expressions

let atomic e =
  match e with
  | Const c -> true
  | Var x -> true
  | _ -> false

let rec normalize term =
  match term with
  | Const c -> term
  | Var x -> term
  | App (f, args)
    -> normalize_name f (fun f ->
        normalize_names args (fun args -> App (f, args)))
  | Abs (args, body) -> Abs (args, normalize body)
  | Let ([], body) -> normalize body
  | Let ((x, e) :: bindings, body) ->
    Let ([x, normalize e], normalize (Let(bindings, body)))
  | Fix f -> Fix (normalize f)
  | Cond (cond, t, f) -> normalize_name cond
                           (fun c ->
                              Cond (c, normalize t, normalize f))
  | Def (var, e) -> Def (var, normalize e)
and normalize_name term k =
  if (atomic term) then
    k term
  else
    let v = Var.gen_var "g" in
    Let ([v, term], k (Var v))
and normalize_names terms k =
  match terms with
  | [] -> k []
  | term :: terms ->
    normalize_name term (fun term ->
                         normalize_names terms (fun terms ->
                                                k (term :: terms)))
