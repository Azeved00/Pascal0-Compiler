{-# LANGUAGE MultiWayIf #-}
module TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map
import Lexer
import Parser


type Env = Map String Type

checkStm :: Env -> Stm -> Bool
checkStm _ _ = True

checkExp :: Env -> Exp -> Type
checkExp _ (Num _)  = TyBasic INTEGER
checkExp _ (Bool _) = TyBasic BOOLEAN
checkExp _ (Str _)  = TyBasic STRING
--checkExp e (Id x)   = e ! x
--checkExp e (BinOp o e1 e2) = case o of
--    PLUS -> if
--        | t1 == INTEGER && t2 == INTEGER -> tyBasic INTEGER
--        | otherwise -> error

-- do what t1 and t2 is
-- do other operations
-- maybe find better sintax 

