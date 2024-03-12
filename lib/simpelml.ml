
let usage = "usage: while-lang [options] file.sm"

let parse_only = ref false

let spec = [
  "--parse-only", Arg.Set parse_only, "  stop after parsing";
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
    let _ = Lexing.from_channel c in
    begin
      try
        while true do
          let achar = input_char c in
          print_char achar;
        done
      with End_of_file -> close_in c
    end