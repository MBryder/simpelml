open Lexer2

let rec read_all_tokens lexbuf =
  match Lexer2.next_tokens lexbuf with
  | [USTOP] -> []
  | tokens -> List.map token_to_string tokens @ read_all_tokens lexbuf

  let test_recognition_all_tokens () =
    let test_tokens input expected =
          let lexbuf = Lexing.from_string input in
          let actual = read_all_tokens lexbuf in
          Alcotest.(check (list string)) "lexer test" expected actual
    in

    test_tokens "+ - * // % = == != < <= > >= ( ) [] , : ^T M* M+ M- .inv .det .scale .pop .push .len return def if else while for in and or not print \n .ustop" ["PLUS"; "MINUS"; "TIMES"; "DIV"; "MOD"; "EQUAL"; "CMP Beq"; "CMP Bneq"; "CMP Blt"; "CMP Ble"; "CMP Bgt"; "CMP Bge"; "LP"; "RP"; "LSQ"; "RSQ"; "COMMA"; "COLON"; "TRANS"; "MTIMES"; "MPLUS"; "MMINUS"; "INV"; "DET"; "SCALE"; "POP"; "PUSH"; "LEN"; "RETURN"; "DEF"; "IF"; "ELSE"; "WHILE"; "FOR"; "IN"; "AND"; "OR"; "NOT"; "PRINT"; "NEWLINE"; "BEGIN"];

    test_tokens "6 6.6 True False .ustop" ["CST (Cint 6)"; "CST (Cfloat 6.6)"; "CST (Cbool true)"; "CST (Cbool false)"];

    test_tokens "testword _testword testword1 testword_test testword_1 _1 _12three .ustop" ["IDENT testword"; "IDENT _testword"; "IDENT testword1"; "IDENT testword_test"; "IDENT testword_1"; "IDENT _1"; "IDENT _12three"]

    

let suite = [
  "Token recognition test", `Quick, test_recognition_all_tokens;
]
