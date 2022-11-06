{-# LANGUAGE MultiWayIf #-}
module TypeCheck where

import DataTypes
import Data.Map (Map)
import qualified Data.Map as Map

type Env = Map String Type
type EnvProc = Map String [Type]

-- helper functions
tcError :: String -> String -> a
tcError m s = error ("Type Error: " ++ m ++ " " ++ s ++ ".")

findEnv :: Env -> String -> Type
findEnv e s = case Map.lookup s e of
                 Just t -> t 
                 Nothing -> tcError s "wasn't undefined"

findProc :: EnvProc -> String -> [Type]
findProc e s = case Map.lookup s e of
                Just t -> t
                Nothing -> tcError s "wasn't defined"

checkType :: Type -> Type -> Bool
checkType (TyBasic t1) (TyBasic t2) = t1 == t2
checkType (TyArray t1 _ _) (TyArray t2 _ _) = t1 == t2

paramType :: Param -> [Type]
paramType EmptyParam = []
paramType (Parameter _ t) = [t]
paramType (CompoundParam p1 p2) = (paramType p1) ++ (paramType p2)


--checkProgram
checkProg :: Prog -> Bool
checkProg (Program _ (Body c p v s)) = 
    checkStm env' envp s
    where env  = constDef Map.empty c
          envp = checkProc env Map.empty p
          env' = varDef env v

--check Procedure
checkProc :: Env -> EnvProc -> Proc -> EnvProc
checkProc env envp (Proc header body) 
        | cond = procDef envp header
        | otherwise = tcError "Procedure was not defined correctly" (show header)
        where cond = checkBody env envp body
checkProc env envp (CompoundProc p1 p2) = 
        checkProc env envp' p2
        where envp' = checkProc env envp p1
checkProc env envp EmptyProc = envp

-- define procedure
procDef :: EnvProc -> ProcHeader -> EnvProc
procDef envp (Procedure s p) = Map.insert s (paramType p) envp
procDef envp (Function s p t) = Map.insert s (t : paramType p) envp

--check body
checkBody :: Env -> EnvProc -> ProcBody -> Bool
checkBody env envp (ProcBody v s) = checkStm env' envp s
                                    where env' = varDef env v
-- define var
varDef :: Env -> Var -> Env
varDef env (Var s t) = Map.insert s t env
varDef env (CompoundVar v1 v2) = varDef env' v2
                                 where env' = varDef env v1
varDef env (EmptyVar) = env

-- define constant
constDef :: Env -> Const -> Env
constDef env (Const s _) = Map.insert s (TyBasic INTEGER) env
constDef env (CompoundConst c1 c2) = constDef env' c2
                                   where env' = constDef env c1
constDef env (EmptyConst) = env

-- check Parameters
checkParam :: Env -> EnvProc -> Exp -> [Type] -> Bool
checkParam _ _ EmptyExp [] = True
checkParam env envp (CompoundExp e1 e2) (t1:t2) = (checkType (checkExp env envp e1) t1)
                                                  && (checkParam env envp e2 t2)
checkParam _ _ (CompoundExp _ _) _ = False
checkParam _ _ _ (_:_:_) = False
checkParam env envp exp [typ] = checkType (checkExp env envp exp) typ

-- Check Statment
checkStm :: Env -> EnvProc -> Stm -> Bool
checkStm env envp (BreakStm) = True
checkStm env envp (CompoundStm stm1 stm2) = checkStm env envp stm1 && checkStm env envp stm2
checkStm env envp (AssignStm (Id id) exp) = if
        | checkType (checkExp env envp exp) typ -> True
        | otherwise -> tcError ("error assigning " ++ (show exp) ++ " to") id 
        where typ = findEnv env id
checkStm env envp (IfStm cond stm) = if 
        | tycond && check -> True
        | otherwise -> tcError "If Stm" 
                ("Condition: " ++ show tycond ++ " Statement: " ++ show check)
        where tycond = checkType (checkExp env envp cond) (TyBasic BOOLEAN)
              check = checkStm env envp stm
checkStm env envp (IfElseStm cond stm1 stm2) = if
        | tycond && check1 && check2 -> True
        | otherwise -> tcError "If Then Else Stm" 
                ("Condition: " ++ show tycond ++ " Statement 1: " ++ show check1 ++ " statement 2: " ++ show check2) 
        where tycond = checkType (checkExp env envp cond) (TyBasic BOOLEAN)
              check1 = checkStm env envp stm1
              check2 = checkStm env envp stm2
checkStm env envp (WhileStm cond stm) = if 
        | tycond && check -> True
        | otherwise -> tcError "While" ("Condition: " ++ show tycond ++ " Statement: " ++ show check)
        where tycond = checkType (checkExp env envp cond) (TyBasic BOOLEAN)
              check = checkStm env envp stm
checkStm env envp (ForStm stm1 cond stm2) = if
        | tycond && check1 && check2 -> True
        | otherwise -> tcError "For" 
                ("Statement 1: " ++ show check1 ++ " Condition: " ++ show tycond ++ " statement 2: " ++ show check2)        
        where tycond = checkType (checkExp env envp cond) (TyBasic BOOLEAN)
              check1 = checkStm env envp stm1
              check2 = checkStm env envp stm2
checkStm env envp (ProcStm name exp) = if 
        | checkParam env envp exp typ -> True
        | otherwise -> tcError "Parameters were incorrect for procedure" name
        where typ = findProc envp name

-- Check Expression
checkExp :: Env -> EnvProc -> Exp -> Type
checkExp _ _ (Num _)  = TyBasic INTEGER
checkExp _ _ (Bool _) = TyBasic BOOLEAN
checkExp _ _ (Str _)     = TyBasic STRING
checkExp e _ (Id x)      = findEnv e x
checkExp ev ep (Array x e) = if
    | checkType (checkExp ev ep e) (TyBasic INTEGER) -> findEnv ev x
    | otherwise -> tcError "Incorrect Array Access" x
checkExp e p (UnOp o e1) = 
    let t = checkExp e p e1 
    in if
        | o == MINUS -> if
            | checkType t int -> int
            | otherwise -> tcError "Wrong Type for operation" (show o)
        | o == NOT -> if
            | checkType t bool -> bool
            | otherwise -> tcError "Wrong Type for operation" (show o)
        | otherwise -> tcError "Operation is not Unary" (show o)
        where int = TyBasic INTEGER
              bool = TyBasic BOOLEAN
checkExp e p (BinOp o e1 e2) = 
    let t1 = checkExp e p e1 
        t2 = checkExp e p e1 
    in if
        | elem o [PLUS, MINUS, MULT, DIV, MOD] -> if
            | checkType t1 int && checkType t2 int -> TyBasic INTEGER
            | otherwise ->  tcError "Wrong Type for operation" (show o)
        | elem o [GREAT, LESS, GEQUAL, LEQUAL, DIFF, EQUAL] -> if
            | checkType t1 int && checkType t2 int -> TyBasic BOOLEAN
            | otherwise -> tcError "Wrong Type for operation" (show o)
        | elem o [AND, OR] -> if
            | checkType t1 bool && checkType t1 bool -> bool
            | otherwise -> tcError "Wrong Type for operation" (show o)
        | otherwise -> tcError "Operation is not Binary" (show o)
        where int = TyBasic INTEGER
              bool = TyBasic BOOLEAN
checkExp ev ep (Func i p) = 
    let types = findProc ep i
        retType = head types
        parType = tail types
    in if
        | checkParam ev ep p parType -> retType 
        | otherwise -> tcError "parameters are not the same for" i 
        
