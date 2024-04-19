open Alcotest
open Lexer
open Lexing


let expr_pp fmt expr = 
  (* Implement a pretty-printer for your expression type here *)
  Fmt.string fmt (string_of_expr expr)

let expr_eq expr1 expr2 =
  (* Implement a comparator for your expression type here *)
  expr1 = expr2

let test () =
  let lexbuf = Lexing.from_string "let x = 1 + 2 in x" in
  let result = Parser.main Lexer.token lexbuf in
  let expected = Let ("x", Add (Int 1, Int 2), Var "x") in
  check (testable expr_pp expr_eq) "same expression" expected result

let suite = [
  "parsing", `Quick, test;
]

let () =
  run "Lexer" suite