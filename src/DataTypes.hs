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

-- Parser Data Type
data Type = TyBasic BasicType
          | TyArray BasicType Exp Exp
          deriving Show

data Prog = Program String ProgBody
          deriving Show

data ProgBody = Body [Const] [Proc] [Var] Stm
              deriving Show

data Stm = AssignStm Exp Exp
         | IfStm Exp Stm
         | IfElseStm Exp Stm Stm
         | WhileStm Exp Stm
         | ForStm Stm Exp Stm
         | BreakStm
         | ProcStm String Exp
         | CompoundStm [Stm]
         deriving Show

data Exp = Num Int
         | Id String
         | Bool Bool
         | Str String
         | BinOp Op Exp Exp
         | RelOp Op Exp Exp
         | UnOp Op Exp
         | Array String Exp
         | Func String Exp
         | CompoundExp [Exp]
         deriving Show

type Const = (String, Int)

type Var = (String, Type)

type Proc = (ProcHeader, ProcBody)

data ProcHeader = Procedure String [Param]
                | Function String [Param] Type
                deriving Show

type Param = (Id, Type)

type ProcBody = ([Var], Stm)

-- Intermadiate Code
type Id = String
type Temp = String
type Label = String

type ICode = ([Def], [Instr])

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | MOVES Temp Label
           | OPER Op Temp Temp Temp
           | OPERI Op Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp Op Temp Label Label
           | CONDI Temp Op Int Label Label
           | CALL Temp Label [Temp]
           | LOAD Temp Int Temp
           | SAVE Temp Int Temp
           | RETURN Temp
           deriving Show

data Def = DARRAY Label Int
         | DSTRING Label String
         deriving Show
