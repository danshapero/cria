
exception TypeCheckFailure of string;;

module StringMap = Map.Make(String)
type type_context = DataTypes.t StringMap.t

let empty_context = StringMap.empty

let add_binding variable datatype context =
  StringMap.add variable datatype context

(* Make this a list fold *)
let rec add_variables variables datatype context =
  match variables with
  | [] -> context
  | x :: variables -> add_variables variables
                                    datatype
                                    (add_binding x datatype context)

let rec add_bindings bindings context =
  match bindings with
  | [] -> context
  | (x, t) :: bindings -> add_bindings bindings (add_binding x t context)


let typeof_constant = function
  | Expr.Nil -> DataTypes.Nil_t
  | Expr.Int _ -> DataTypes.Int_t
  | Expr.Float _ -> DataTypes.Float_t
  | Expr.Bool _ -> DataTypes.Bool_t

let typeof_variable x context =
  StringMap.find x context

let rec typeof e context =
  match e with
  | Expr.Const a -> typeof_constant a
  | Expr.Var x -> typeof_variable x context
  | Expr.App (f, args) -> typeof_application f args context
  | Expr.Abs (args, body) -> typeof_abstraction args body context
  | Expr.Let (bindings, body) -> typeof_let bindings body context
  | Expr.Fix f -> typeof_fix f context
  | Expr.Cond (condition, true_branch, false_branch) ->
    if (typeof condition context) = DataTypes.Bool_t then
      let t_true_branch = typeof true_branch context
      and t_false_branch = typeof false_branch context in
      if t_true_branch = t_false_branch then
        t_true_branch
      else
        raise (TypeCheckFailure "Types of conditional branches don't match!")
    else
      raise (TypeCheckFailure "Condition not a boolean!")
  | Expr.Def (_, e) -> typeof e context

and typeof_application f args context =
  let arg_types = List.map (fun x -> typeof x context) args in
  match (typeof f context) with
  | DataTypes.Function_t (f_arg_types, ret_type) ->
    if arg_types = f_arg_types then
      ret_type
    else
      raise (TypeCheckFailure "Arg types did not match return type!")
  | _ -> raise (TypeCheckFailure "First expr in application not a function!")

and typeof_abstraction args body context =
  let context = add_bindings args context
  and arg_types = List.map (fun p -> snd p) args in
  let body_type = typeof body context in
  DataTypes.Function_t (arg_types, body_type)

and typeof_let bindings body context =
  let context' = List.fold_left
                   (fun ctxt (x, e) -> add_binding x (typeof e context) ctxt)
                   context
                   bindings
  in
  typeof body context'

and typeof_fix f context =
  match typeof f context with
  | DataTypes.Function_t ([arg], ret) ->
     if arg = ret then
       ret
     else
       raise (TypeCheckFailure
                "Arg/return types of function passed to Fix do not match.")
  | _ -> raise (TypeCheckFailure "Argument to fix is not a function!")


let rec typecheck program context =
  match program with
  | [] -> context
  | e :: program ->
    let t = typeof e context in
    let context =
      (match e with
       | Expr.Def (x, body) -> add_binding x t context
       | _ -> context)
    in
    typecheck program context
