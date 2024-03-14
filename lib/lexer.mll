
(* Lexical analyzer for While Language. *)
{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string


  let id_or_kwd =
    let h = Hashtbl.create 32 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok)
      [
       "if", IF; "else", ELSE;
       "print", PRINT;
       "while", WHILE;
       "and", AND; "or", OR; "not", NOT;
       "True", CST (Cbool true);
       "False", CST (Cbool false);
     ];
   fun s -> try Hashtbl.find h s with Not_found -> IDENT s

  let string_buffer = Buffer.create 1024

  let stack = ref [0]  (* indentation stack *)

  let rec unindent n = match !stack with
    | m :: _ when m = n -> []
    | m :: st when m > n -> stack := st; END :: unindent n
    | _ -> raise (Lexing_error "bad indentation")

  let update_stack n =
    match !stack with
    | m :: _ when m < n ->
      stack := n :: !stack;
      [NEWLINE; BEGIN]
    | _ ->
      NEWLINE :: unindent n
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_') (letter | digit | '_')*
let integer = '0' | ['1'-'9'] digit*
let space = ' ' | '\t'
let comment = "#" [^'\n']*

rule next_tokens = parse
  | '\n'    { new_line lexbuf; update_stack (indentation lexbuf) }
  | (space | comment)+
            { next_tokens lexbuf }
  | ident as id { [id_or_kwd id] }
  | '+'     { [PLUS] }
  | '-'     { [MINUS] }
  | '*'     { [TIMES] }
  | "//"    { [DIV] }
  | '%'     { [MOD] }
  | '='     { [EQUAL] }
  | "=="    { [CMP Beq] }
  | "!="    { [CMP Bneq] }
  | "<"     { [CMP Blt] }
  | "<="    { [CMP Ble] }
  | ">"     { [CMP Bgt] }
  | ">="    { [CMP Bge] }
  | '('     { [LP] }
  | ')'     { [RP] }
  | ','     { [COMMA] }
  | ':'     { [COLON] }
  | integer as s
            { try [CST (Cint (int_of_string s))]
              with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | '"'     { [CST (Cstring (string lexbuf))] }
  | eof     { NEWLINE :: unindent 0 @ [EOF] }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and indentation = parse
  | (space | comment)* '\n'
      { new_line lexbuf; indentation lexbuf }
  | space* as s
      { String.length s }

and string = parse
  | '"'
      { let s = Buffer.contents string_buffer in
	Buffer.reset string_buffer;
	s }
  | "\\n"
      { Buffer.add_char string_buffer '\n';
	string lexbuf }
  | "\\\""
      { Buffer.add_char string_buffer '"';
	string lexbuf }
  | _ as c
      { Buffer.add_char string_buffer c;
	string lexbuf }
  | eof
      { raise (Lexing_error "unterminated string") }
{
  let next_token =
  Printf.printf "Token: ";
  let tokens = Queue.create () in (* next tokens to emit *)
  fun lb ->
    (* Fill the queue if it's empty *)
    if Queue.is_empty tokens then begin
      let l = next_tokens lb in
      List.iter (fun t -> Queue.add t tokens) l
    end;
  let t = Queue.pop tokens in
    (* Processing the token *)
    begin match t with
      | NEWLINE -> Printf.printf "\nToken: "
      | _ -> Printf.printf "[%s], " (token_to_string t)
    end;
    t (* Returning the token *)




    let token_to_string = function
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | TIMES -> "TIMES"
    | DIV -> "DIV"
    | MOD -> "MOD"
    | EQUAL -> "EQUAL"
    | CMP cmp ->
        begin match cmp with
        | Beq -> "CMP Beq"
        | Bneq -> "CMP Bneq"
        | Blt -> "CMP Blt"
        | Ble -> "CMP Ble"
        | Bgt -> "CMP Bgt"
        | Bge -> "CMP Bge"
        end
    | LP -> "LP"
    | RP -> "RP"
    | COMMA -> "COMMA"
    | COLON -> "COLON"
    | CST cst ->
        begin match cst with
        | Cint i -> "CST (Cint " ^ string_of_int i ^ ")"
        | Cbool b -> "CST (Cbool " ^ string_of_bool b ^ ")"
        | Cstring s -> "CST (Cstring " ^ s ^ ")"
        end
    | IDENT id -> "IDENT " ^ id
    | IF -> "IF"
    | ELSE -> "ELSE"
    | WHILE -> "WHILE"
    | AND -> "AND"
    | OR -> "OR"
    | NOT -> "NOT"
    | PRINT -> "PRINT"
    | NEWLINE -> "NEWLINE"
    | BEGIN -> "BEGIN"
    | END -> "END"
    | EOF -> "EOF"
    | _ -> "Unknown token"

}

