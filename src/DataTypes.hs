--Define the grammar for Pascal-0
module DataTypes where

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
        | UMINUS
        deriving(Eq, Show)

data BasicType = INTEGER
               | BOOLEAN
               | DOUBLE
               | STRING
               deriving(Eq, Show)

data Token = IF
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
