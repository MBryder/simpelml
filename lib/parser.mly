
/* Parser for While */

%{
  open Ast
%}

%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <string> IDENT
%token DEF RETURN FUNC IF ELSE PRINT WHILE FOR IN AND OR NOT ELIF
%token EOF
%token LP RP LSQ RSQ COMMA EQUAL COLON BEGIN END NEWLINE
%token PLUS MINUS TIMES DIV MOD

/* priorities and associativities */

%left OR
%left AND
%nonassoc NOT
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus

%start file
%type <Ast.file> file

%%

file:
| NEWLINE? b = nonempty_list(stmt) NEWLINE? EOF
    { Sblock b }
;

def: 
| DEF f = indent LP x = separated_list(COMMA, ident) RP  (* her definerer vi funktion f, med lP som holder parameter listen til functionen og RP. *)
  COLON s = suite (* suite er en block af kode så s er altså selve indholdet til funktionen*)
   {f, x, s} (* skal vi inkoperere return her? det behøver man jo ikke altid så det skal nok defineresin the interpreter or something*)
; 

expr:
| c = CST
    { Ecst c }
| id = ident
    { Eident id }
| e1 = expr LSQ e2 = expr RSQ
    { Eget (e1, e2) }
| MINUS e1 = expr %prec unary_minus
    { Eunop (Uneg, e1) }
| NOT e1 = expr
    { Eunop (Unot, e1) }
| e1 = expr o = binop e2 = expr
    { Ebinop (o, e1, e2) }
| f = ident LP e = separated_list(COMMA, expr) RP (*definerer functionen*)
    {Ecall (f,e)}
| LP e = expr RP
    { e }
| LSQ l = separated_list(COMMA, expr) RSQ
    { Elist l }
;

suite:
| s = simple_stmt NEWLINE
    { s }
| NEWLINE BEGIN l = nonempty_list(stmt) END
    { Sblock l }
;

stmt:
| s = simple_stmt NEWLINE
    { s }
| IF c = expr COLON s = suite
    { Sif (c, s, Sblock []) }
| IF c = expr COLON s1 = suite ELSE COLON s2 = suite
    { Sif (c, s1, s2) }
| WHILE e = expr COLON s = suite
    { Swhile (e, s) }
| FOR id = IDENT EQUAL e1 = expr IN e2 = expr COLON s = suite
    { Sfor ({loc = ($startpos, $endpos); id = id}, e1, e2, s) }
;

simple_stmt:
| RETURN e = expr 
    {Sreturn e}
| id = ident EQUAL e = expr
    { Sassign (id, e) }
| id = ident PLUS EQUAL e = expr
    { Sassign (id, Ebinop (Badd, Eident id, e)) }
| PRINT LP el = separated_list(COMMA, expr) RP
    { Sprint el }
;

%inline binop:
| PLUS  { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV   { Bdiv }
| MOD   { Bmod }
| c=CMP { c    }
| AND   { Band }
| OR    { Bor  }
;

ident:
  id = IDENT { { loc = ($startpos, $endpos); id } }
;
