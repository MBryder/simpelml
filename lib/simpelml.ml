open Format
open Lexing
open Parser
let usage = "usage: while-lang [options] file.sm"

let parse_only = ref false

let spec = [
  "--parse-only", Arg.Set parse_only, "  stop after parsing";
]

let file =
  let file = ref None in  (* tjekker om vores fil som vi tager som input slutter med .sm*)
  let set_file s =
    if not (Filename.check_suffix s ".sm") then
      raise (Arg.Bad "no .sm extension");
    file := Some s
  in
  Arg.parse spec set_file usage; 
  
  match !file with (* her tjekker vi om filen faktisk er en fil, altså matcher f med f og ellers exiter vi programmet med exitkode 1*)
  | Some f -> f
  | None -> Arg.usage spec usage; exit 1


let report (b,e) = 
  let l = b.pos_lnum in (* sætter første linje til linje 1 *)
  let fc = b.pos_cnum - b.pos_bol + 1 in (*fc og lc beregner begge lokationer i koden *)
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc (* printer en fejlbesked med lokationenerne i koden hvor fejlen opstår*)

let () = 
    let c = open_in file in (* her åbner vi bare filen og definerer c som den åbne fil*)
    let lb = Lexing.from_channel c in (*laver lexing buffer lb. lb encapsulerer inputtet, og giver os mulighed for at vide hvor i inputtet 
       et specifikt symbol er, og vores lexer læser characters fra lb og konstruere tokens baseret på lb's input og vores grammatik *)
    try
      let f = Parser.file Lexer.next_token lb in
      close_in c;
      if !parse_only then exit 0;
      Interp.file f
    with
    | Lexer.Lexing_error s ->
      report (lexeme_start_p lb, lexeme_end_p lb); (* når vi kalder report funktionen beregner vi lokationen for fejlen i koden*)
      eprintf "lexical error: %s@." s;
      exit 1
    | Parser.Error ->
      report (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "syntax error@.";
      exit 1
    | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
    