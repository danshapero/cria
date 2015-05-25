
open OUnit
open Expressions
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Core.Std.fprintf outx "%s:%d:%d" pos.pos_fname
                   pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    Core.Std.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    Core.Std.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let expr_of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

module ExprDict = Map.Make(String)

let add_expr s dict =
  match expr_of_string s with
    | Some e -> ExprDict.add s e dict
    | None   -> dict

let correct_parse string_expression correct_expression =
  match expr_of_string string_expression with
    | Some expression -> expression = correct_expression
    | None            -> false

let test_fixture = "Expression parser" >:::
[
  "atoms" >::
    ( fun () -> assert_equal true (correct_parse "1" (Constant (Int 1)))
    )
]

let _ =
  run_test_tt_main test_fixture
