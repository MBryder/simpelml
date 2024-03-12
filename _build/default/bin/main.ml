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
  let lb = Lexing.from_channel c in  (* Create a lexing buffer from the input channel *)
  try
    while true do
      let token = Lexer.next_token lb in  (* Read the next token from the lexing buffer *)
      (* Process the token as needed; for example, print it out or perform some other action. *)
      (* This loop will keep running until an exception is raised, typically the end of file. *)
      ()
    done
  with
  | End_of_file -> close_in c  (* Close the file when the end of the file is reached. *)
  | Lexer.Lexing_error s ->  (* Handle lexing errors if your lexer can raise them. *)
      Printf.eprintf "lexical error: %s\n" s;
      exit 1
