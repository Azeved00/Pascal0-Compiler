{
-- Analisador sintático para a calculadora simples
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token

id { IDENT $$ }
num { NUM $$ }
bool { BOOL $$ }
str { STR $$ }
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
'.' { DOT }
':' { DDOT }
',' { COMMA }
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
const { CONST }
var { VAR }
type { TYPE $$ }
array { ARRAY }
of { OF }
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

ProcDecls : Proc ProcDecls                                             { CompoundProc $1 $2 }
          | {- empty -}                                                { EmptyProc }


-- Procedures and Functions
Proc : ProcHeader ProcBody ';'                                         { Proc $1 $2 }

ProcHeader : procedure id '(' ParamList ')' ';'                        { Procedure $2 $4 }
           | function id '(' ParamList ')' ':' BasicType ';'           { Function $2 $4 (TyBasic $7) }

ParamList : ParamList1                                                 { $1 }
          | {- empty -}                                                { EmptyParam }

ProcBody : VarDecls CompoundStm                                        { ProcBody $1 $2 }

ParamList1 : Param ';' ParamList1                                      { CompoundParam $1 $3 }
           | Param                                                     { $1 }

Param : id ':' Type                                                    { Parameter $1 $3 }


-- Declarations
ConstDecls : const ConstDefSeq                                         { $2 }
           | {- empty -}                                               { EmptyConst }

VarDecls : var VarDefSeq                                               { $2 }
         | {- empty -}                                                 { EmptyVar }

ConstDef : id '=' num ';'                                              { Const $1 $3 }

ConstDefSeq : ConstDef ConstDefSeq                                     { CompoundConst $1 $2 }
            | ConstDef                                                 { $1 }

VarDef : id ':' Type ';'                                               { Var $1 $3 }

VarDefSeq : VarDef VarDefSeq                                           { CompoundVar $1 $2 }
          | VarDef                                                     { $1 }

Type : BasicType                                                       { TyBasic $1 }
     | ArrayType                                                       { $1 }

BasicType : type                                                       { $1 }

ArrayType : array '[' Constant '.' '.' Constant ']' of BasicType       { TyArray $9 $3 $6 }

Constant : num                                                         { Num $1 }
         | id                                                          { Id $1 }


-- Statements
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

AssignStm : VarAcess ':=' Exp                   { AssignStm $1 $3 }

IfStm : if Exp then Stm                         { IfStm $2 $4 }
      | if Exp then Stm else Stm                { IfElseStm $2 $4 $6 }


WhileStm : while Exp do Stm                     { WhileStm $2 $4 }

ForStm : for AssignStm to Exp do Stm            { ForStm $2 $4 $6 }

BreakStm : break                                { BreakStm }

ProcStm : id '(' ExpList ')'                    { ProcStm $1 $3 }

CompoundStm : begin StmList end                 { $2 }


-- Expressions
ExpList : ExpList1              { $1 }
        | {- empty -}           { EmptyExp }

ExpList1 : Exp ',' ExpList1     { CompoundExp $1 $3 }
         | Exp                  { $1 }

VarAcess : id                   { Id $1 }
         | id '[' Exp ']'       { Array $1 $3 }

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
      | '(' Exp ')'             { $2 }
      | id '(' ExpList ')'      { Func $1 $3 }
      | num                     { Num $1 }
      | bool                    { Bool $1 }
      | str                     { Str $1 }
      | VarAcess                { $1 }

{
data Type = TyBasic BasicType
          | TyArray BasicType Exp Exp
          deriving Show

data Prog = Program String ProgBody
          deriving Show

data ProgBody = Body Const Proc Var Stm
              deriving Show

data Stm = AssignStm Exp Exp
         | IfStm Exp Stm
         | IfElseStm Exp Stm Stm
         | WhileStm Exp Stm
         | ForStm Stm Exp Stm
         | BreakStm
         | ProcStm String Exp
         | CompoundStm Stm Stm
         | EmptyStm
         deriving Show

data Exp = Num Int
         | Id String
         | Bool Bool
         | Str String
         | BinOp Op Exp Exp
         | UnOp Op Exp
         | Array String Exp
         | Func String Exp
         | CompoundExp Exp Exp
         | EmptyExp
         deriving Show

data Const = CompoundConst Const Const
           | Const String Int
           | EmptyConst
           deriving Show

data Var = CompoundVar Var Var
         | Var String Type
         | EmptyVar
         deriving Show

data Proc = Proc ProcHeader ProcBody
          | CompoundProc Proc Proc
          | EmptyProc
          deriving Show

data ProcHeader = Procedure String Param
                | Function String Param Type
                deriving Show

data Param = CompoundParam Param Param
           | Parameter String Type
           | EmptyParam
           deriving Show

data ProcBody = ProcBody Var Stm
              deriving Show



parseError :: [Token] -> a
parseError toks = error ("parse error" ++ (show (head toks)) ++ " " ++ show (take 10 toks) ++ " " ++ show (length toks))
}
