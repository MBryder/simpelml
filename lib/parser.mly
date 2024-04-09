
/* Parser for simpleML */
/* Parsing sker efter vi har anvendt vores lexer. Den arbejder altså med de tokens, som vores lexer har produceret. Pointen med parseren 
er at analysere strukturen i kilde koden, i relation til grammatiken. Dette gør parseren ved at konstruere vores abstract syntax tree, 
som altså giver os en hierakisk forståelse af kildekoden */ 

%{
  open Ast
%}

%token <Ast.constant> CST
%token <Ast.binop> CMP
%token <string> IDENT
%token IF ELSE PRINT WHILE FOR IN AND OR NOT
%token EOF
%token LP RP LSQ RSQ COMMA EQUAL COLON BEGIN END NEWLINE
%token PLUS MINUS TIMES DIV MOD TRANS MTIMES INV

/* priorities and associativities */

%nonassoc TRANS INV
%left OR
%left AND
%nonassoc NOT
%nonassoc CMP
%left PLUS MINUS
%left TIMES DIV MOD MTIMES
%nonassoc unary_minus

%start file
%type <Ast.file> file

%%

file:
| NEWLINE? b = nonempty_list(stmt) NEWLINE? EOF
    { Sblock b }
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
| e1 = expr TRANS
    { Eunop (Utrans, e1) }
| e1 = expr INV
    { Eunop (Uinv, e1) }
| e1 = expr o = binop e2 = expr
    { Ebinop (o, e1, e2) }
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
| e1 = expr LSQ e2 = expr RSQ EQUAL e3 = expr NEWLINE
      { Slist_assign (e1, e2, e3) }
;

simple_stmt:
| id = ident EQUAL e = expr
    { Sassign (id, e) }
| id = ident PLUS PLUS
    { Sincr (id)}
| id = ident MINUS MINUS
    { Sdecr (id)}
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
| MTIMES { Bmtimes }
;

ident:
  id = IDENT { { loc = ($startpos, $endpos); id } }
;
