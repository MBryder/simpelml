
(* Abstract Syntax of simpleML. *)
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
     | Utrans (* trans e *)
     | Uinv (* e  inv *)
     | Udet (* e det *)
     | Uscale of float (* scale e *)
     | Upop   (* e.pop() *)
     | Ulen   (* e.len *)

   (* Binary operators. *)
   type binop =
     | Badd | Bsub | Bmul | Bdiv | Bmod | Bmtimes | Bmplus | Bmminus (* + - * // % *)
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
     | Ecall of ident * expr list         
     

   
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
     | Sreturn of expr                 
     | Seval of expr  

  and def = ident * ident list * stmt 

   (* a program is simply a statement. *)
  and file = def list * stmt

(* Pretty printer for AST *)
let string_of_ident = function
| {id=id; _} -> id

let string_of_unop = function
| Uneg -> "-"
| Unot -> "not"
| Utrans -> "trans"
| Uinv -> "inv"
| Udet -> "det"
| Uscale f -> "scale " ^ string_of_float f
| Upop -> "pop"
| Ulen -> "len"

let string_of_binop = function
| Badd -> "+"
| Bsub -> "-"
| Bmul -> "*"
| Bdiv -> "/"
| Bmod -> "%"
| Bmtimes -> "mtimes"
| Bmplus -> "mplus"
| Bmminus -> "mminus"
| Beq -> "=="
| Bneq -> "!="
| Blt -> "<"
| Ble -> "<="
| Bgt -> ">"
| Bge -> ">="
| Band -> "and"
| Bor -> "or"

let string_of_constant = function
| Cbool b -> string_of_bool b
| Cstring s -> "\"" ^ s ^ "\""
| Cint i -> string_of_int i
| Cfloat f -> string_of_float f

let rec pretty_print_expr = function
| Ecst c -> string_of_constant c
| Ebinop (op, e1, e2) -> pretty_print_expr e1 ^ " " ^ string_of_binop op ^ " " ^ pretty_print_expr e2
| Eunop (op, e) -> string_of_unop op ^ pretty_print_expr e
| Ecall (id, es) -> string_of_ident id ^ "(" ^ String.concat ", " (List.map pretty_print_expr es) ^ ")"
| Elist es -> "[" ^ String.concat ", " (List.map pretty_print_expr es) ^ "]"
| Eget (e1, e2) -> pretty_print_expr e1 ^ "[" ^ pretty_print_expr e2 ^ "]"
| Eident id -> string_of_ident id

and pretty_print_stmt = function
| Sif (e, s1, s2) -> "if (" ^ pretty_print_expr e ^ ") " ^ pretty_print_stmt s1 ^ " else " ^ pretty_print_stmt s2
| Sassign (id, e) -> string_of_ident id ^ " = " ^ pretty_print_expr e
| Sblock stmts -> "{\n" ^ String.concat "\n" (List.map pretty_print_stmt stmts) ^ "\n}"
| Sprint exprs -> "print(" ^ String.concat ", " (List.map pretty_print_expr exprs) ^ ")"
| Swhile (e, s) -> "while (" ^ pretty_print_expr e ^ ") " ^ pretty_print_stmt s
| Sfor (id, e1, e2, s) -> "for (" ^ string_of_ident id ^ " = " ^ pretty_print_expr e1 ^ "; " ^ string_of_ident id ^ " < " ^ pretty_print_expr e2 ^ ") " ^ pretty_print_stmt s
| Sincr id -> string_of_ident id ^ "++"
| Sdecr id -> string_of_ident id ^ "--"
| Spush (e1, e2) -> "push(" ^ pretty_print_expr e1 ^ ", " ^ pretty_print_expr e2 ^ ")"
| Slist_assign (e1, e2, e3) -> pretty_print_expr e1 ^ "[" ^ pretty_print_expr e2 ^ "] = " ^ pretty_print_expr e3
| Sreturn e -> "return " ^ pretty_print_expr e
| Seval e -> pretty_print_expr e

and pretty_print_def (id, id_list, stmt) = 
  "def " ^ string_of_ident id ^ "(" ^ String.concat ", " (List.map string_of_ident id_list) ^ ") " ^ pretty_print_stmt stmt

and pretty_print_file (def_list, stmt) = 
String.concat "\n" (List.map pretty_print_def def_list) ^ "\n" ^ pretty_print_stmt stmt
