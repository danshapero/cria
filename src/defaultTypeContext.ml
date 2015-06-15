
open DataTypes
open Expressions
open TypeChecker

let iarithmetic = ["+"; "-"; "*"; "/"; "^"; "%"]
let icomparison = ["<"; ">"; ">="; "<="; "="]
let logical_binary = ["and"; "or"]
let logical_unary = ["not"]
let input_output = ["printi"]

let default_context =
  let ctxt = add_variables input_output
                           (Function_t ([Int_t], Int_t))
                           empty_context
  in
  let ctxt = add_variables iarithmetic
                           (Function_t ([Int_t; Int_t], Int_t))
                           ctxt
  in
  let ctxt = add_variables icomparison
                           (Function_t ([Int_t; Int_t], Bool_t))
                           ctxt
  in
  let ctxt = add_variables logical_binary
                           (Function_t ([Bool_t; Bool_t], Bool_t))
                           ctxt
  in
  add_variables logical_unary
                (Function_t ([Bool_t], Bool_t))
                ctxt

