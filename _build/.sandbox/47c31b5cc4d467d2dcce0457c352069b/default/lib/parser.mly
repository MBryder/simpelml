

/* Syntax analyzer for mini-Turtle */

%{
  open Ast

%}

/* Token declaration */

%token <int> CST
%token <string> IDENT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token FORWARD
%token EOF

/* Priorities et associativity laws for tokens */
%left MINUS PLUS
%left TIMES DIV    /* Important that TIMES comes after MINUS and PLUS */

/* Entry point of the grammar */
%start prog

/* The Type of the value returned by the syntax analysis */
%type <Ast.program> prog

%%

/* Grammar rules */

prog:
  main = stmt*
  EOF
    { { defs = [];
        main = Sblock main; } };

stmt:
| Ident


expr:
| c = CST                        { Econst c }
| e1 = expr o = op e2 = expr     { Ebinop (o, e1, e2) }
;

%inline op:
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
;