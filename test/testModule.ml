open Alcotest
open Lexer

let rec read_all_tokens lexbuf =
  match Lexer.next_tokens lexbuf with
  | [USTOP] -> []
  | tokens -> List.map token_to_string tokens @ read_all_tokens lexbuf

  let test_lexer_operator () =
    let test_token input expected =
          let lexbuf = Lexing.from_string input in
          let actual = read_all_tokens lexbuf in
          Alcotest.(check (list string)) "lexer test" expected actual
    in
    (* Tester ikke, om lexeren recognizer: end, begin og EOF *)

    (* Test til alle tokens undtagen int, float, boolean og ident *)
    test_token "+ - * // % = == != < <= > >= ( ) [] , : ^T M* M+ M- .inv .det .scale .pop .push .len return def if else while for in and or not print \n .ustop" ["PLUS"; "MINUS"; "TIMES"; "DIV"; "MOD"; "EQUAL"; "CMP Beq"; "CMP Bneq"; "CMP Blt"; "CMP Ble"; "CMP Bgt"; "CMP Bge"; "LP"; "RP"; "LSQ"; "RSQ"; "COMMA"; "COLON"; "TRANS"; "MTIMES"; "MPLUS"; "MMINUS"; "INV"; "DET"; "SCALE"; "POP"; "PUSH"; "LEN"; "RETURN"; "DEF"; "IF"; "ELSE"; "WHILE"; "FOR"; "IN"; "AND"; "OR"; "NOT"; "PRINT"; "NEWLINE"; "BEGIN"];
    (* Test til int, float and boolean *)
    test_token "6 6.6 True False .ustop" ["CST (Cint 6)"; "CST (Cfloat 6.6)"; "CST (Cbool true)"; "CST (Cbool false)"];
    (* Ident tests *)
    test_token "testword _testword testword1 testword_test testword_1 _1 _12three .ustop" ["IDENT testword"; "IDENT _testword"; "IDENT testword1"; "IDENT testword_test"; "IDENT testword_1"; "IDENT _1"; "IDENT _12three"]

    
(* Alcotest formaliteter *)
let suite = [
  "test_lexer_operator", `Quick, test_lexer_operator; (* Should pass with flying colors *)
]

let () =
  run "My Test Suite" [("Lexer tests", suite)]