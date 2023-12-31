{
module Parser where

import DataTypes
}

%name parse
%tokentype { Token }
%error { parseError }

%token

id      { IDENT $$ }
num     { NUM $$ }
bool    { BOOL $$ }
str     { STR $$ }
'+'     { OP PLUS }
'-'     { OP MINUS }
'*'     { OP MULT }
div     { OP DIV }
mod     { OP MOD }
'>'     { OP GREAT }
'<'     { OP LESS }
'>='    { OP GEQUAL }
'<='    { OP LEQUAL }
'<>'    { OP DIFF }
'='     { OP EQUAL }
and     { OP AND }
or      { OP OR }
not     { OP NOT }
'('     { LPARENT }
')'     { RPARENT }
';'     { SEMICOLON }
'['     { LBRACKET }
']'     { RBRACKET }
':='    { ASSIGN }
'.'     { DOT }
':'     { DDOT }
','     { COMMA }
if      { IF }
then    { THEN }
else    { ELSE }
while   { WHILE }
do      { DO }
for     { FOR }
to      { TO }
break   { BREAK }
begin   { BEGIN }
end     { END }
const   { CONST }
var     { VAR }
type    { TYPE $$ }
array   { ARRAY }
of      { OF }
program { PROGRAM }
procedure { PROCEDURE }
function { FUNCTION }


%right then
%nonassoc '<>' '=' '>' '<' '<=' '>='
%left '+' '-' or
%left '*' div mod and
%left not
%left if
%left else

%%

-- Programs
Program : ProgramHeader ProgramBody '.'                                { Program $1 $2 }

ProgramHeader : program id ';'                                         { $2 }

ProgramBody : ConstDecls ProcDecls VarDecls CompoundStm                { Body $1 $2 $3 $4}

ProcDecls : Proc ProcDecls                                             { $1 : $2 }
          | {- empty -}                                                { [] }


-- Procedures and Functions
Proc : ProcHeader ProcBody ';'                                         { ($1, $2) }

ProcHeader : procedure id '(' ParamList ')' ';'                        { Procedure $2 $4 }
           | function id '(' ParamList ')' ':' BasicType ';'           { Function $2 $4 (TyBasic $7) }

ParamList : ParamList1                                                 { $1 }
          | {- empty -}                                                { [] }

ProcBody : VarDecls CompoundStm                                        { ($1, $2) }

ParamList1 : Param ';' ParamList1                                      { $1 : $3 }
           | Param                                                     { [$1] }

Param : id ':' Type                                                    { ($1, $3) }


-- Declarations
ConstDecls : const ConstDefSeq                                         { $2 }
           | {- empty -}                                               { [] }

VarDecls : var VarDefSeq                                               { $2 }
         | {- empty -}                                                 { [] }

ConstDef : id '=' num ';'                                              { ($1, $3) }

ConstDefSeq : ConstDef ConstDefSeq                                     { $1 : $2 }
            | ConstDef                                                 { [$1] }

VarDef : id ':' Type ';'                                               { ($1, $3) }

VarDefSeq : VarDef VarDefSeq                                           { $1 : $2 }
          | VarDef                                                     { [$1] }

Type : BasicType                                                       { TyBasic $1 }
     | ArrayType                                                       { $1 }

BasicType : type                                                       { $1 }

ArrayType : array '[' Constant '.' '.' Constant ']' of BasicType       { TyArray $9 $3 $6 }

Constant : num                                                         { Num $1 }
         | id                                                          { Id $1 }


-- Statements
StmList : Stm ';' StmList                       { $1 : $3 }
        | Stm                                   { [$1] }

Stm : AssignStm                                 { $1 }
    | IfStm                                     { $1 }
    | WhileStm                                  { $1 }
    | ForStm                                    { $1 }
    | BreakStm                                  { $1 }
    | ProcStm                                   { $1 }
    | CompoundStm                               { $1 }

AssignStm : VarAcess ':=' Exp                   { AssignStm $1 $3 }

IfStm : if Exp then Stm                         { IfStm $2 $4 }
      | if Exp then Stm else Stm                { IfElseStm $2 $4 $6 }


WhileStm : while Exp do Stm                     { WhileStm $2 $4 }

ForStm : for AssignStm to Exp do Stm            { ForStm $2 $4 $6 }

BreakStm : break                                { BreakStm }

ProcStm : id '(' ExpList ')'                    { ProcStm $1 $3 }

CompoundStm : begin StmList end                 { CompoundStm $2 }


-- Expressions
ExpList : ExpList1              { CompoundExp $1 }
        | {- empty -}           { CompoundExp [] }

ExpList1 : Exp ',' ExpList1     { $1 : $3 }
         | Exp                  { [$1] }

VarAcess : id                   { Id $1 }
         | id '[' Exp ']'       { Array $1 $3 }

Exp   : Exp '+' Exp             { BinOp PLUS $1 $3 }
      | Exp '-' Exp             { BinOp MINUS $1 $3 }
      | Exp '*' Exp             { BinOp MULT $1 $3 }
      | Exp div Exp             { BinOp DIV $1 $3 }
      | Exp mod Exp             { BinOp MOD $1 $3 }
      | Exp '>' Exp             { RelOp GREAT $1 $3 }
      | Exp '<' Exp             { RelOp LESS $1 $3 }
      | Exp '>=' Exp            { RelOp GEQUAL $1 $3 }
      | Exp '<=' Exp            { RelOp LEQUAL $1 $3 }
      | Exp '<>' Exp            { RelOp DIFF $1 $3 }
      | Exp '=' Exp             { RelOp EQUAL $1 $3 }
      | Exp and Exp             { BinOp AND $1 $3 }
      | Exp or Exp              { BinOp OR $1 $3 }
      | not Exp                 { UnOp NOT $2 }
      | '-' Exp %prec not       { UnOp MINUS $2 }
      | '(' Exp ')'             { $2 }
      | id '(' ExpList ')'      { Func $1 $3 }
      | num                     { Num $1 }
      | bool                    { Bool $1 }
      | str                     { Str $1 }
      | VarAcess                { $1 }

{
parseError :: [Token] -> a
parseError toks = error ("parse error" ++ (show (head toks)) ++ " " ++ show (take 10 toks) ++ " " ++ show (length toks))
}
