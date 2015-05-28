
open Expressions

exception TypeCheckFailure of string;;

module StringMap = Map.Make(String)
type type_context = data_type StringMap.t

let empty_context = StringMap.empty

let add_context variable datatype context =
  StringMap.add variable datatype context

let add_contexts variables datatype context =
  List.fold_left (fun (var context) ->
                  add_context var datatype context)
                  context
                  variables

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
  | Application (f, args) ->
    let arg_types = List.map (fun (x) -> typeof x context) args in
    match (typeof f context) with
    | Function_t (f_arg_types, ret_type) ->
      if arg_types = f_arg_types
        ret_type
      else
        raise (TypeCheckFailure "Arg types did not match return type!")
    | _ -> raise (TypeCheckFailure "First expr in application not a function!")
  | Abstraction (* Write this *)
  | Let (* This too *)
  | Letrec (* and this *)
  | Conditional (* aaaand this *)
  | _ -> raise (TypeCheckFailure "Nope!")
