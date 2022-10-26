--Define the grammar for Pascal-0
module DataTypes where

data AritOp = PLUS
           | MINUS
           | MULT
           | DIV
           | MOD
           deriving(Eq, Show)

data BoolOp = GREATER
            | LESS
            | GEQUAL
            | LEQUAL
            | NOTEQAL
            | EQUAL
            | AND
            | OR
            | NOT
            deriving(Eq, Show)

data BasicType = INTEGER
               | BOOLEAN  
               | DOUBLE
               | STRING
               | ARRAY
               deriving(Eq, Show)

data Token = IF
           | BREAK
           | IDENT String 
           | TYPE BasicType
           | NUM Int
           | REAL Double
           | STR String
           | BOOL Bool
           | CONST 
           | VAR 
           | ELSE
           | BOOLOP BoolOp
           | ARITOP AritOp
           | SEMICOLON
           | DDOT
           | DOT
           | ASSIGN
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

