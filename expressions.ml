
type variable = string

type constant =
    | Int of int
    | Float of float
    | Bool of bool


type data_type = Bool_t
               | Int_t
               | Float_t
               | Function_t of data_type list

type expr =
    | Constant of constant
    | Variable of variable
    | Application of expr * expr list
    | Abstraction of (variable * data_type) list * expr
    | Let of (variable * data_type * expr) list * expr
    | Letrec of (variable * data_type * expr) list * expr
    | Conditional of (expr * expr * expr)


let string_of_constant c =
  match c with
  | Int i   -> string_of_int i
  | Float x -> string_of_float x
  | Bool p  -> string_of_bool p


let rec string_of_data_type t =
  match t with
    | Bool_t        -> "bool"
    | Int_t         -> "int"
    | Float_t       -> "float"
    | Function_t ts -> let arg_strings = List.map string_of_data_type ts in
                       "(-> " ^ (String.concat " " arg_strings) ^ ")"


let rec string_of_expr expr =
  let string_of_binding (name, ty, expr) =
    name ^ ":" ^ (string_of_data_type ty) ^ " " ^ (string_of_expr expr)
  in
  let string_of_arg_decl (name, ty) =
    name ^ ":" ^ (string_of_data_type ty)
  in
  match expr with
    | Constant c            -> string_of_constant c
    | Variable v            -> v
    | Application (f, args) -> let sf = string_of_expr f in
                               let sa = List.map string_of_expr args in
                               "(" ^ sf ^ " " ^ (String.concat " " sa) ^ ")"
    | Abstraction (args, e) -> let s = string_of_expr e in
                               let a = List.map string_of_arg_decl args in
                               "(lambda (" ^ (String.concat " " a) ^ ") " ^ s ^ ")"
    | Let (bindings, e)     -> let s = string_of_expr e in
                               let b = List.map string_of_binding bindings in
                               "(let (" ^ (String.concat " " b) ^ ") " ^ s ^ ")"
    | Letrec (bindings, e)  -> let s = string_of_expr e in
                               let b = List.map string_of_binding bindings in
                               "(letrec (" ^ (String.concat " " b) ^ ") " ^ s ^ ")"
    | Conditional (cond, t, f)       -> "(if " ^ (string_of_expr cond)
                                        ^ " " ^ (string_of_expr t)
                                        ^ " " ^ (string_of_expr f) ^ ")"
