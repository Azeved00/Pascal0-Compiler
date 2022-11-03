module TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map
import Lexer
import Parser


type Env = Map String Type

checkStm :: Env -> Stm -> Bool
checkStm _ _ = True

checkExp :: Env -> Exp -> Type
checkExp _ _ = TyBasic INTEGER 
