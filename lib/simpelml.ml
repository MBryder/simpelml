open Format
open Lexing
open Parser
open Typechecker
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
  let c = open_in file in
  let lb = Lexing.from_channel c in
  let env = Hashtbl.create 10 in (* Initialize an empty environment *)
  try
    let (defs, main_stmt) = Parser.file Lexer.next_token lb in
    close_in c;

    (* Type check all definitions and the main statement *)
    List.iter (fun (f, args, body) -> ignore (type_of_stmt env body)) defs;
    ignore (type_of_stmt env main_stmt);
    eprintf "Typechecking complete.@.";

    if not !parse_only then
      Interp.file (defs, main_stmt)  (* Interpret the program if parsing only is not set *)
  with
  | Lexer.Lexing_error s ->
      report (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s@." s;
      exit 1
  | Parser.Error ->
      report (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "syntax error@.";
      exit 1
  | TypeError (msg, _) ->
      eprintf "Type error: %s@." msg;
  | e ->
      eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2