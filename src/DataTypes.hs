module DataTypes where


-- Lexer data types
data Op = PLUS
        | MINUS
        | MULT
        | DIV
        | MOD
        | GREAT
        | LESS
        | GEQUAL
        | LEQUAL
        | DIFF
        | EQUAL
        | AND
        | OR
        | NOT
        deriving(Eq, Show)

data BasicType = INTEGER
               | BOOLEAN
               | STRING
               deriving(Eq, Show)

data Token  = IF
            | THEN
            | BREAK
            | ASSIGN
            | IDENT String
            | TYPE BasicType
            | NUM Int
            | REAL Double
            | STR String
            | BOOL Bool
            | ARRAY
            | CONST
            | VAR
            | ELSE
            | OP Op
            | COMMA
            | SEMICOLON
            | DDOT
            | DOT
            | LBRACKET
            | RBRACKET
            | LPARENT
            | RPARENT
            | WHILE
            | DO
            | BEGIN
            | END
            | FOR
            | TO
            | OF
            | PROGRAM
            | FUNCTION
            | PROCEDURE
            deriving (Eq,Show)

-- Parser Data Types
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
         | CompoundStm [Stm]
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
         | CompoundExp [Exp]
         deriving Show

data Const = CompoundConst [Const]
           | Const String Int
           deriving Show

data Var = CompoundVar [Var]
         | Var String Type
         deriving Show

data Proc = Proc ProcHeader ProcBody
          | CompoundProc [Proc]
          deriving Show

data ProcHeader = Procedure String Param
                | Function String Param Type
                deriving Show

data Param = CompoundParam [Param]
           | Parameter String Type
           deriving Show

data ProcBody = ProcBody Var Stm
              deriving Show

-- Intermadiate Code

type Temp = String
type Label = String
type Id = String

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | OPER Binop Temp Temp Temp
           | OPERI Binop Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Exp Label Label
           | CALL Temp Id [Temp]
           | RETURN Temp
