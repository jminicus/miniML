/*
                         CS 51 Final Project
                           MiniML -- Parser
*/

%{
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG
%token PLUS MINUS
%token TIMES
%token LESSTHAN EQUALS
%token IF THEN ELSE
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT
/* added support for strings, float, and try / with statements */
%token <string> STRING
%token <float> FLOAT
%token TRY
%token WITH
%token TRUE FALSE
%token CONCAT
%token LENGTH

%nonassoc LESSTHAN
%nonassoc EQUALS
%left PLUS MINUS
%left TIMES
%left NEG

%start input
%type <Expr.expr> input

/* Grammar follows */
%%
input:  exp EOF                 { $1 }

exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

expnoapp: INT                   { Num $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | STRING                { String $1 }
        | FLOAT                 { Float $1 }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | NEG exp               { Unop(Negate, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp   { Fun($2, $4) }
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }
        /* added support for string operations, curried functions
           with one arg (let and let rec), and try / with */
        | exp CONCAT exp        { Binop(Concat, $1, $3) }
        | LENGTH exp            { Unop(Length, $2) }
        | LET ID ID EQUALS exp IN exp   { Let($2, (Fun($3, $5)), $7) }
        | LET REC ID ID EQUALS exp IN exp
                                { Letrec($3, (Fun($4, $6)), $8) }
        | TRY exp WITH exp      { Try($2, $4) }
;

%%
