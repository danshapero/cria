
open Expressions

exception TypeCheckFailure of string;;

module StringMap = Map.Make(String)
type type_context = data_type StringMap.t

let empty_context = StringMap.empty

let add_context variable datatype context =
  StringMap.add variable datatype context

(* Make this a list fold *)
let rec add_contexts variables datatype context =
  match variables with
  | [] -> context
  | x :: variables -> add_contexts variables
                                   datatype
                                   (add_context x datatype context)

let rec add_vars decls context =
  match decls with
  | [] -> context
  | (x, t) :: decls -> add_vars decls (add_context x t context)

let default_context =
  let ctxt =
    add_contexts ["+"; "-"; "*"; "/"]
                 (Function_t ([Int_t; Int_t],
                               Int_t))
                 empty_context in
  let ctxt =
    add_contexts ["and"; "or"]
                 (Function_t ([Bool_t; Bool_t],
                              Bool_t))
                 ctxt in
  add_context "not"
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
(*  | Let (bindings, body) -> (* write this *)
  | Letrec (* and this *)
  | Conditional (* aaaand this *) *)
  | _ -> raise (TypeCheckFailure "Nope!")
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
  let context = add_vars args context in
  let body_type = typeof body context in
  if body_type = ret_type then
    let arg_types = List.map (fun p -> snd p) args in
    Function_t (arg_types, ret_type)
  else
    raise (TypeCheckFailure "Declared/inferred function body type mismatch!")
