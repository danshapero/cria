
open DataTypes
open Expressions

exception NormalizeFail;;

let atomic e =
  match e with
  | Const c -> true
  | Var x -> true
  | _ -> false

let count = ref 0
let fresh_variable () =
  count := !count + 1;
  "#g-" ^ (string_of_int !count)

let id x = x

let rec normalize term =
  match term with
  | Const c -> term
  | Var x -> term
  | App (f, args)
    -> normalize_name f (fun f->
        normalize_names args (fun args -> App (f, args)))
  (*-> normalize_name f (fun t -> normalize_names args (fun ts -> t :: ts))*)
  | Abs (args, body) -> Abs (args, normalize body)
  | Let (bindings, body) -> normalize_let bindings body
  | Fix f -> Fix (normalize f)
  | Cond (cond, t, f) -> normalize_name cond
                           (fun c ->
                              Cond (c, normalize t, normalize f))
  | Def (var, e) -> Def (var, normalize e)
and normalize_let bindings body =
  match bindings with
  | [] -> normalize body
  | (x, e) :: bindings -> normalize_let ((x, normalize e) :: bindings) body
and normalize_name term k =
  if (atomic term) then
    k term
  else
    let t = fresh_variable () in
    Let ([t, term], k (Var t))
and normalize_names terms k =
  match terms with
  | [] -> k []
  | term :: terms ->
    normalize_name term (fun term ->
                         normalize_names terms (fun terms ->
                                                k (term :: terms)))
