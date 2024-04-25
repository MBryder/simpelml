open Alcotest
open Lexer

let rec read_all_tokens lexbuf =
  match Lexer.next_tokens lexbuf with
  | [IDENT _stop] -> []
  | tokens -> List.map token_to_string tokens @ read_all_tokens lexbuf

  let test_lexer_operator () =
    let test_token input expected =
          let lexbuf = Lexing.from_string input in
          let actual = read_all_tokens lexbuf in
          Alcotest.(check (list string)) "lexer test" expected actual
    in
    test_token "5 * 5 _stop" ["CST (Cint 5)"; "TIMES"; "CST (Cint 5)"];
    test_token "M+ _stop" ["MPLUS"]

let test_lexer_operator_fake () =
  let lexbuf = Lexing.from_string "TRANS" in (* "test-filen" *) 
    let expected = "TRANS" in (* Expected  *) 
    let actual = 
    match Lexer.next_tokens lexbuf with
    | tokens -> List.hd (List.map token_to_string tokens)
  in
  check Alcotest.string "lexer test" expected actual (* Assertion om EXPECTED er lig ACTUAL *)  

    
(* Alcotest formaliteter *)
let suite = [
  "test_lexer_operator", `Quick, test_lexer_operator; (* Should pass with flying colors *)
  "test_lexer_operator_fake", `Quick, test_lexer_operator_fake; (*Should Fail*)
]

let () =
  run "My Test Suite" [("Lexer tests", suite)]