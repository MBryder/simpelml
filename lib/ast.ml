
(* Abstract Syntax of simpleML. *)

(* Parsed trees.
   This is the output of the parser and the input of the interpreter. 
   Den her fil indeholder altså definitioner på elementer og operationer, relateret til vores abstract syntax tree som bliver skabt i 
   parser.mly*)



(* Identifiers. *)

(* Identifiers (variables) are normally represented by a value of type
   `string` but we wrap them into the following record type `ident`, using
   additional field `loc` that tracks the position (line & column) for each
   identifier. That is done to signal better syntax errors, e.g. when
   an unknown identifier is used in the program. *)
   type location = Lexing.position * Lexing.position
   type ident = { loc: location; id: string; }
   
   (* Unary operators. *)
   type unop =
     | Uneg (* -e *)
     | Unot (* not e *)
     | Uinv (* inv e *)
     | Utrans (* e^T *)
     | Upop   (* e.pop() *)
     | Ulen   (* e.len *)
   (* Binary operators. *)
   type binop =
     | Badd | Bsub | Bmul | Bdiv | Bmod | Bmtimes   (* + - * // % *)
     | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
     | Band | Bor                          (* and or *)
   
   (* Constants. *)
   type constant =
     | Cbool of bool
     | Cstring of string
     | Cint of int
     | Cfloat of float
   
   (* Expressions. *)
   type expr =
     | Ecst of constant                   (* constant *)
     | Eunop of unop * expr               (* unary operation *)
     | Ebinop of binop * expr * expr      (* binary operation *)
     | Eident of ident                    (* variable *)
     | Elist of expr list                 (* [e1,e2,...] *)
     | Eget of expr * expr                (* e1[e2] *)
     | Ecall of ident * expr list         (*funktionskald med parametre*)
     

   
   (* Statements. *)
   type stmt =
     | Sif of expr * stmt * stmt       (* conditional *)
     | Sassign of ident * expr         (* modifying a variable *)
     | Sblock of stmt list             (* a sequence of statements *)
     | Sprint of expr list             (* printing a list of expressions *)
     | Swhile of expr * stmt           (* while loop *)
     | Sfor of ident * expr * expr * stmt (* for loop *)
     | Sincr of ident
     | Sdecr of ident
     | Spush of expr * expr
     | Slist_assign of expr * expr * expr  (* New statement type: array, index, new value *)
     | Sreturn of expr                 (* til funktion*)
     | Seval of expr  

  and def = ident * ident list * stmt (*definerer function *)

   (* a program is simply a statement. *)
  and file = def list * stmt
   