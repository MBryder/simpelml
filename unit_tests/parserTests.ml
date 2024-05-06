open Alcotest
open Parser

let test_parser () =
let tokens = ref [DEF; IDENT "x"; EQUAL; CST(Cint 1); NEWLINE; PRINT; LP; IDENT "x"; RP; NEWLINE] in
let next_token _ =
  match !tokens with
  | [] -> EOF
  | hd :: tl -> tokens := tl; hd
in
let actual_ast = Parser.file next_token (Lexing.from_string "") in
let actual_ast_str = Ast.pretty_print_file actual_ast in
let expected_ast_str = "print()" in
Alcotest.(check string) "print()" expected_ast_str actual_ast_str

let suite = [
  test_case "ast equality test" `Quick test_parser;
]