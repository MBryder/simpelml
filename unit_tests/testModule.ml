let () =
  Alcotest.run "My Test Suite" [
    "Lexer tests", LexerTests.suite;
    "Parser tests", ParserTests.suite;
  ]