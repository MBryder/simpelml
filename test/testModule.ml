open Alcotest
open Lexer
open Parser

(* Skriv om, så kan modtage en string, der indeholder flere tokens *)
(* Lige nu kan testen kun modtage en string, der indeholder én token *)

let test_lexer () =
  let lexbuf = Lexing.from_string "+" in
  let expected = "PLUS" in
  let actual = 
    match Lexer.next_tokens lexbuf with
    | [EOF] -> ""
    | tokens -> List.hd (List.map token_to_string tokens)
  in
  check Alcotest.string "lexer test" expected actual

let suite = [
  "test_lexer", `Quick, test_lexer;
]

let () =
  run "My Test Suite" [("Lexer tests", suite)]