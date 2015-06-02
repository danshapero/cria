
open Expressions

exception TypeCheckFailure of string;;

module StringMap = Map.Make(String)
type type_context = data_type StringMap.t

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

let default_context =
  let ctxt =
    add_variables ["+"; "-"; "*"; "/"]
                  (Function_t ([Int_t; Int_t],
                               Int_t))
                  empty_context in
  let ctxt =
    add_variables [">"; "<"]
                  (Function_t ([Int_t; Int_t],
                               Bool_t))
                  ctxt in
  let ctxt =
    add_variables ["and"; "or"]
                  (Function_t ([Bool_t; Bool_t],
                               Bool_t))
                  ctxt in
  add_binding "not"
              (Function_t ([Bool_t],
                           Bool_t))
              ctxt


let typeof_constant a =
  match a with
  | Int _ -> Int_t
  | Float _ -> Float_t
  | Bool _ -> Bool_t

let typeof_variable x context =
  StringMap.find x context


let rec typeof e context =
  match e with
  | Constant a -> typeof_constant a
  | Variable x -> typeof_variable x context
  | Application (f, args) -> typeof_application f args context
  | Abstraction (args, ret_type, body) -> typeof_abstraction args ret_type body context
  | Let (bindings, body) -> typeof_let bindings body context
  | Letrec (bindings, body) -> typeof_letrec bindings body context
  | Conditional (condition, true_branch, false_branch) ->
     if (typeof condition context) = Bool_t then
       let t_true_branch = typeof true_branch context
       and t_false_branch = typeof false_branch context in
       if t_true_branch = t_false_branch then
         t_true_branch
       else
         raise (TypeCheckFailure "Types of conditional branches don't match!")
     else
       raise (TypeCheckFailure "Condition not a boolean!")

and typeof_application f args context =
  let arg_types = List.map (fun x -> typeof x context) args in
  match (typeof f context) with
  | Function_t (f_arg_types, ret_type) ->
    if arg_types = f_arg_types then
      ret_type
    else
      raise (TypeCheckFailure "Arg types did not match return type!")
  | _ -> raise (TypeCheckFailure "First expr in application not a function!")

and typeof_abstraction args ret_type body context =
  let context = add_bindings args context in
  let body_type = typeof body context in
  if body_type = ret_type then
    let arg_types = List.map (fun p -> snd p) args in
    Function_t (arg_types, ret_type)
  else
    raise (TypeCheckFailure "Declared/inferred function body type mismatch!")

and typeof_let bindings body context =
  let rec check_let_bindings bindings context =
    match bindings with
    | [] -> context
    | (x, t, e) :: bindings
      -> let context = add_binding x t context in
         if (typeof e context) = t then
           check_let_bindings bindings context
         else
           raise (TypeCheckFailure
                    "Declared/inferred let binding type mismatch!")
  in
  typeof body (check_let_bindings bindings context)

and typeof_letrec bindings body context =
  let rec add_letrec_bindings bindings context =
    match bindings with
    | [] -> context
    | (x, t, _) :: bindings
      -> add_letrec_bindings bindings (add_binding x t context)
  and check_letrec_bindings bindings context =
    match bindings with
    | [] -> true
    | (_, t, e) :: bindings
      -> if (typeof e context) = t then
           check_letrec_bindings bindings context
         else
           false
  in
  let context = add_letrec_bindings bindings context in
  if check_letrec_bindings bindings context then
    typeof body context
  else
    raise (TypeCheckFailure
             "Declared/inferred letrec binding type mismatch!")
