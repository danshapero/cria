
type atom =
  | Symbol of string
  | Integer of int
  | Real of float
  | Boolean of bool
  | Character of char

type expr = Atom of atom
          | Expr of expr list

