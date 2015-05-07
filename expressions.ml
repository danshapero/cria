
type atom = Symbol of string
          | Integer of int
          | Real of float
          | Boolean of bool
          | Character of char

type expr = Atom of atom
          | Expression of expr list

type data_type = SymbolType
               | IntegerType
               | RealType
               | BooleanType
               | CharacterType
               | ListType of data_type
               | FunctionType of data_type * data_type

let is_abstraction e =
  match e with
  | Atom _ -> false
  | Expression ex -> (List.hd ex) = Atom (Symbol "lambda")

let is_let e =
  match e with
  | Atom _ -> false
  | Expression ex -> (List.hd ex) = Atom (Symbol "let")

let is_conditional e =
  match e with
  | Atom _ -> false
  | Expression ex -> (List.hd ex) = Atom (Symbol "if")

exception TypeCheckFailure of string;;

module StringMap = Map.Make(String)
type type_context = data_type StringMap.t

let empty_context = StringMap.empty

let add_context variable datatype context =
  StringMap.add variable datatype context

let rec add_contexts variables datatype context =
  match variables with
    | [] -> context
    | x :: vars -> add_contexts vars datatype (add_context x datatype context)

let default_context =
  let ctxt =
    add_contexts ["+"; "-"; "*"; "/"]
                 (FunctionType (IntegerType,
                               FunctionType (IntegerType, IntegerType)))
                 empty_context in
  let ctxt =
    add_contexts ["and"; "or"]
                 (FunctionType (BooleanType,
                               FunctionType (BooleanType, BooleanType)))
                 ctxt in
  add_context "not"
              (FunctionType (BooleanType, BooleanType))
              ctxt


let typeof_atom a =
  match a with
  | Symbol _ -> SymbolType
  | Integer _ -> IntegerType
  | Real _ -> RealType
  | Boolean _ -> BooleanType
  | Character _ -> CharacterType

let rec typeof e context =
  match e with
  | Atom a -> typeof_atom a
  | _ -> raise (TypeCheckFailure "Nope!")
