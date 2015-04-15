
type atom = Symbol of string
          | Integer of int32
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
               | FunctionType of (data_type list) * (data_type list)

StringMap = Map.Make(String)

type type_table = data_type StringMap.t

let print_atom a =
  match a with
    Symbol a -> Printf.printf "%s\n" a
  | Integer a -> Printf.printf "%ld\n" a
  | Real a -> Printf.printf "%g\n" a
  | Boolean a -> Printf.printf "%B\n" a
  | Character a -> Printf.printf "hey";;

let rec print_expr a =
  match a with
    Atom a -> print_atom a
  | Expression a -> let fir = List.hd a in
                    print_expr fir;;


let q = Symbol "+" in
    print_atom q;;
