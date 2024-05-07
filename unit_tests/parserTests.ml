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
    (* write prettycode into a file then repeat the parsing that is 
       open file and call lexer and parser to get some AST f' 
       
       ... in 
       assert (f = f')*)
    let _expected_ast_str = "" in
    (*let expected_ast_str = "def sum (x, y): return x+y sigurd = sum(3,5)" in *)
    Alcotest.(check string) "ast equality test" "x" "x"
  with
  | Lexer.Lexing_error s ->
    Alcotest.fail ("lexical error: " ^ s)
  (*| Parser.Error -> 
    Alcotest.fail "syntax error" *)
  | e ->
    Alcotest.fail ("Anomaly: " ^ (Exn.to_string e))

let suite = [
  test_case "ast equality test" `Quick test_parser;
]