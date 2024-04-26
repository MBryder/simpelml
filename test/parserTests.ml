open Alcotest
open Parser
open Lexing
open Lexer
open Ast

let pp_constant = function
  | Cint n -> string_of_int n
  | Cfloat f -> string_of_float f
  | _ -> failwith "Unsupported expression type"
  (* Add more cases here for other constant types *)

let pp_unop = function
  | Uneg -> "-"
  | Unot -> "not"
  | _ -> failwith "Unsupported expression type"
  (* Add more cases here for other unary operator types *)

let pp_binop = function
  | Badd -> "+"
  | Bsub -> "-"
  | Bmul -> "*"
  | Bdiv -> "/"
  | _ -> failwith "Unsupported expression type"
  (* Add more cases here for other binary operator types *)

(* AST -> string *)
let rec pp_expr = function
  | Ecst n -> pp_constant n
  | Eunop (op, e) -> pp_unop op ^ " " ^ pp_expr e
  | Ebinop (op, e1, e2) -> "(" ^ pp_expr e1 ^ " " ^ pp_binop op ^ " " ^ pp_expr e2 ^ ")"
  | _ -> failwith "Unsupported expression type"

(* Check if AST's are equal at each node *)
let rec equal_expr e1 e2 =
    match (e1, e2) with
    | (Ecst n1, Ecst n2) -> n1 = n2
    | (Eunop (op1, e1), Eunop (op2, e2)) -> op1 = op2 && equal_expr e1 e2
    | (Ebinop (op1, e1a, e1b), Ebinop (op2, e2a, e2b)) -> op1 = op2 && equal_expr e1a e2a && equal_expr e1b e2b
    | _ -> false

let pp_expr_fmt = Fmt.of_to_string pp_expr

(* Parse a string into an AST, with help of lexer *)
let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.file Lexer.next_token lexbuf

  let test_parser () =
    let expected = Ebinop (Badd, Ecst (Cint 2), Ecst (Cint 3)) in
    let lexbuf = Lexing.from_string "2 + 3" in
    let actual = Parser.Expr_start Lexer.next_tokens lexbuf in
    check (testable pp_expr_fmt equal_expr) "parser test" expected actual

let suite = [
  test_case "parser test" `Quick test_parser;
]