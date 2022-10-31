{
-- Analisador sintático para a calculadora simples
module Parser where
import Lexer
import DataTypes
}

%name parse
%tokentype { Token }
%error { parseError }

%token

id { IDENT $$ }
num { NUM $$ }
bool { BOOL $$ }
'+' { OP PLUS }
'-' { OP MINUS }
'*' { OP MULT }
div { OP DIV }
mod { OP MOD }
'>' { OP GREAT }
'<' { OP LESS }
'>=' { OP GEQUAL }
'<=' { OP LEQUAL }
'<>' { OP DIFF }
'=' { OP EQUAL }
and { OP AND }
or { OP OR }
not { OP NOT }
'(' { LPARENT }
')' { RPARENT }
';' { SEMICOLON }
'[' { LBRACKET }
']' { RBRACKET }
':=' { ASSIGN }
if { IF }
then { THEN }
else { ELSE }
while { WHILE }
do { DO }
for { FOR }
to { TO }
break { BREAK }
begin { BEGIN }
end { END }


%right then
%nonassoc '<>' '=' '>' '<' '<=' '>='
%left '+' '-' or
%left '*' div mod and
%left not
%%

StmList : Stm ';' StmList                       { CompoundStm $1 $3 }
        | Stm                                   { $1 }

Stm : AssignStm                                 { $1 }
    | IfStm                                     { $1 }
    | WhileStm                                  { $1 }
    | ForStm                                    { $1 }
    | BreakStm                                  { $1 }
    | ProcStm                                   { $1 }
    | CompoundStm                               { $1 }
    | {- empty -}                               { EmptyStm }

AssignStm : id ':=' Exp                         { AssignStm $1 $3 }

IfStm : CloseIf                                 { $1 }
      | OpenIf                                  { $1 }

OpenIf : if Exp then Stm                         { IfStm $2 $4 }
       | if Exp then CloseIf else OpenIf         { IfElseStm $2 $4 $6 }

CloseIf : if Exp then CloseIf else CloseIf       { IfElseStm $2 $4 $6 }

WhileStm : while Exp do Stm                     { WhileStm $2 $4 }

ForStm : for AssignStm to Exp do Stm            { ForStm $2 $4 $6 }

BreakStm : break                                { BreakStm }

ProcStm : id '(' ExpList ')'                    { ProcStm $1 $3 }

CompoundStm : begin StmList end                 { $2 }

ExpList : ExpList1              { $1 }
        | {- empty -}           { EmptyExp }

ExpList1 : Exp ';' ExpList1     { CompoundExp $1 $3 }
         | Exp                  { $1 }

Exp   : Exp '+' Exp             { BinOp PLUS $1 $3 }
      | Exp '-' Exp             { BinOp MINUS $1 $3 }
      | Exp '*' Exp             { BinOp MULT $1 $3 }
      | Exp div Exp             { BinOp DIV $1 $3 }
      | Exp mod Exp             { BinOp MOD $1 $3 }
      | Exp '>' Exp             { BinOp GREAT $1 $3 }
      | Exp '<' Exp             { BinOp LESS $1 $3 }
      | Exp '>=' Exp            { BinOp GEQUAL $1 $3 }
      | Exp '<=' Exp            { BinOp LEQUAL $1 $3 }
      | Exp '<>' Exp            { BinOp DIFF $1 $3 }
      | Exp '=' Exp             { BinOp EQUAL $1 $3 }
      | Exp and Exp             { BinOp AND $1 $3 }
      | Exp or Exp              { BinOp OR $1 $3 }
      | not Exp                 { UnOp NOT $2 }
      | '-' Exp %prec not       { UnOp MINUS $2 }
      | id '(' ExpList ')'      { Function $1 $3 }
      | id '[' Exp ']'          { Array $1 $3 }
      | num                     { Num $1 }
      | id                      { Id $1 }
      | bool                    { Bool $1 }
{
type Ident = String

data Stm = AssignStm Ident Exp
         | IfStm Exp Stm
         | IfElseStm Exp Stm Stm
         | WhileStm Exp Stm
         | ForStm Stm Exp Stm
         | BreakStm
         | ProcStm Ident Exp
         | CompoundStm Stm Stm
         | EmptyStm
         deriving Show

data Exp = Num Int
         | Id Ident
         | Bool Bool
         | BinOp Op Exp Exp
         | UnOp Op Exp
         | Array Ident Exp
         | Function Ident Exp
         | CompoundExp Exp Exp
         | EmptyExp
         deriving Show

parseError :: [Token] -> a
parseError toks = error "parse error"
}
