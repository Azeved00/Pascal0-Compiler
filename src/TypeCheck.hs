module TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map
import Lexer
import Parser

type Env = Map String Type
type EnvProc = Map String [Type]

checkProg :: Prog -> Bool
checkProg (Program _ pbody) = checkProgBody pbody

checkProgBody :: ProgBody -> Bool
checkProgBody (Body c p v s) = checkStm env' envp s
                               where env = constDef Map.empty c
                                     envp = checkProc env Map.empty p
                                     env' = varDef env v

checkProc :: Env -> EnvProc -> Proc -> EnvProc
checkProc env envp (Proc header body) | cond = procDef envp header
                                      | otherwise = error "type error"
                                      where cond = checkBody env envp body

checkProc env envp (CompoundProc p1 p2) = checkProc env envp' p2
                                          where envp' = checkProc env envp p1
checkProc env envp EmptyProc = envp

procDef :: EnvProc -> ProcHeader -> EnvProc
procDef envp (Procedure s p) = Map.insert s (paramType p) envp
procDef envp (Function s p t) = Map.insert s (t : paramType p) envp

paramType :: Param -> [Type]
paramType EmptyParam = []
paramType (Parameter _ t) = [t]
paramType (CompoundParam p1 p2) = (paramType p1) ++ (paramType p2)

checkBody :: Env -> EnvProc -> ProcBody -> Bool
checkBody env envp (ProcBody v s) = checkStm env' envp s
                                    where env' = varDef env v

varDef :: Env -> Var -> Env
varDef env (Var s t) = Map.insert s t env
varDef env (CompoundVar v1 v2) = varDef env' v2
                                 where env' = varDef env v1
varDef env (EmptyVar) = env

constDef :: Env -> Const -> Env
constDef env (Const s _) = Map.insert s (TyBasic INTEGER) env
constDef env (CompoundConst c1 c2) = constDef env' c2
                                   where env' = constDef env c1
constDef env (EmptyConst) = env

checkType :: Type -> Type -> Bool
checkType (TyBasic t1) (TyBasic t2) = t1 == t2
checkType (TyArray t1 _ _) (TyArray t2 _ _) = t1 == t2


checkParam :: Env -> EnvProc -> Exp -> [Type] -> Bool
checkParam _ _ EmptyExp [] = True
checkParam env envp (CompoundExp e1 e2) (t1:t2) = (checkType (checkExp env envp e1) t1)
                                                  && (checkParam env envp e2 t2)
checkParam _ _ (CompoundExp _ _) _ = False
checkParam _ _ _ (_:_:_) = False
checkParam env envp exp [typ] = checkType (checkExp env envp exp) typ

checkStm :: Env -> EnvProc -> Stm -> Bool
checkStm env envp (AssignStm (Id id) exp) = case Map.lookup id env of
                                            Just typ -> if checkType (checkExp env envp exp) typ then True
                                                        else error "type error in assign"
                                            Nothing -> error "undeclared variable"

checkStm env envp (IfStm cond stm) = if checkType tycond (TyBasic BOOLEAN) && check then True
                                     else error "type error in if then else"
                                     where tycond = checkExp env envp cond
                                           check = checkStm env envp stm

checkStm env envp (IfElseStm cond stm1 stm2) = if checkType tycond (TyBasic BOOLEAN) && check1 && check2 then True
                                               else error "type error in if then else"
                                               where tycond = checkExp env envp cond
                                                     check1 = checkStm env envp stm1
                                                     check2 = checkStm env envp stm2


checkStm env envp (WhileStm cond stm) = if checkType tycond (TyBasic BOOLEAN) && check then True
                                        else error "type error in while"
                                        where tycond = checkExp env envp cond
                                              check = checkStm env envp stm

checkStm env envp (ForStm stm1 cond stm2) = if checkType tycond (TyBasic BOOLEAN) && check1 && check2 then True
                                            else error "type error in while"
                                            where tycond = checkExp env envp cond
                                                  check1 = checkStm env envp stm1
                                                  check2 = checkStm env envp stm2

checkStm env envp (BreakStm) = True

checkStm env envp (ProcStm name exp) = case Map.lookup name envp of
                                            Just typ -> if checkParam env envp exp typ then True
                                                        else error "type error in procedure call"
                                            Nothing -> error "undeclared procedure"

checkStm env envp (CompoundStm stm1 stm2) = checkStm env envp stm1 && checkStm env envp stm2



checkExp :: Env -> EnvProc -> Exp -> Type
checkExp _ _ (Num _) = TyBasic INTEGER
checkExp _ _ (Bool _) = TyBasic BOOLEAN
checkExp _ _ _ = TyBasic STRING
