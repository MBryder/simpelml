let lexer =
  SimpelML.Lexer.from_string "const a = 52;\nmut b = 7;\nb=8;\n\nalpha_print a;"

let rec print_token_until_invalid lexer =
  let token = SimpelML.Lexer.next_token lexer in
  print_endline ("Token: " ^ SimpelML.Token.to_string token);
  match token with
  | Invalid -> print_endline "Invalid!!!"
  | EOF -> ()
  | _ -> print_token_until_invalid lexer
;;

print_token_until_invalid lexer
