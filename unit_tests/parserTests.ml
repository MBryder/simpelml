open Alcotest
open Core

let test_parser () =
  let file_name = "unit_tests/parserTestInput.sm" in
  if not (Filename.check_suffix file_name ".sm") then
    raise (Arg.Bad "no .sm extension");
  let c = In_channel.create file_name in

  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.next_token lb in
    In_channel.close c;

    (* Pretty print the AST *)
    let pretty_code = Ast.pretty_print_file f in
    print_endline pretty_code;

    let expected_ast_str = "print()" in
    Alcotest.(check string) "ast equality test" expected_ast_str pretty_code
  with
  | Lexer.Lexing_error s ->
    Alcotest.fail ("lexical error: " ^ s)
  | Parser.Error ->
    Alcotest.fail "syntax error"
  | e ->
    Alcotest.fail ("Anomaly: " ^ (Exn.to_string e))

let suite = [
  test_case "ast equality test" `Quick test_parser;
]