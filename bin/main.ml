open Format
open Lexing
open Parser

let usage = "usage: while-lang [options] file.sm"

let parse_only = ref false

let spec = [
  ("--parse-only", Arg.Set parse_only, "  Stop after parsing");
]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".sm") then
      raise (Arg.Bad "no .sm extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  
  match !file with
  | Some f -> f
  | None -> Arg.usage spec usage; exit 1

  let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.next_token lb in
    close_in c;
    if !parse_only then exit 0;
    Interp.file f
  with
    | Lexer.Lexing_error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | Interp.Error s ->
	eprintf "error: %s@." s;
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2