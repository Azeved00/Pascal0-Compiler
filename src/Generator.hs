module Generator where

import DataTypes
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map



generate :: Prog -> [Instr]
generate p =  evalState (genInstr p) (0,0)

type Count = (Int,Int)
type Table = Map Id Temp

genInstr :: Prog -> State Count [Instr]
genInstr (Program _ (Body const procs vars prog)) = do
    t1 <- loadConsts Map.empty const 
    procs <- loadProcs t1 procs 
    t2 <- loadVars t1 vars
    list <- transStm t2 "" prog
    return (procs ++ [LABEL "Main"]++list)

-------------- Load Stuff ------------------------------------
loadVars :: Table -> [Var] -> State Count Table
loadVars t [] = return (t)
loadVars t ((id,_):vs) = do
    temp <- newTemp
    nt <- loadVars (Map.insert id temp t) vs
    return (nt)

loadConsts :: Table -> [Const] -> State Count Table
loadConsts t [] = return (t)
loadConsts t ((id,_):vs) = do
    temp <- newTemp
    nt <- loadConsts (Map.insert id temp t) vs 
    return (nt)

loadParams :: Table -> [Param] -> State Count Table
loadParams t [] = return (t)
loadParams t ((id,_):xs) = do
    temp <- newTemp
    ntab <- loadParams (Map.insert id temp t) xs
    return (ntab)


loadProcs :: Table -> [Proc] -> State Count [Instr]
loadProcs tab []= return ([])
loadProcs tab (((Procedure l params),(vrs,stms)):xs) = do
    ntab1   <- loadParams tab params 
    ntab2   <- loadVars ntab1 vrs
    endl    <- newLabel
    stcode  <- transStm ntab2 l stms
    other   <- loadProcs tab xs 
    return ([LABEL l] ++ stcode ++ other)

loadProcs tab (((Function id params tpe),(vrs,stms)):xs) = do
    ntab1   <- loadParams tab params
    ntab2   <- loadVars ntab1 ((id,tpe):vrs)
    endl    <- newLabel
    fcode   <- transStm ntab2 endl stms
    other   <- loadProcs tab xs
    return ([LABEL id] ++ fcode ++[RETURN id] ++ other)
------------------------auxiliar functions---------------------------------

newTemp :: State Count Temp
newTemp = do (t,l)<-get; put (t+1,l); return ("t"++show t) 
popTemp :: Int -> State Count ()
popTemp k =  modify (\(t,l) -> (t-k,l))

newLabel :: State Count Label
newLabel = do (t,l)<-get; put (t,l+1); return ("L"++show l)

-------------------- Trans Exp-------------------------------------------
transExp :: Table -> Exp -> Temp -> State Count [Instr]
transExp tab (Num n) dest = do return [MOVEI dest n]
transExp tab (Id s) dest = case Map.lookup s tab of
    Just temp -> return [MOVE dest temp]
    Nothing -> error ("Error:" ++ show s ++ " invalid variable")

transExp tab (BinOp op e1 e2) dest
    = do    t1 <- newTemp
            t2 <- newTemp
            code1 <- transExp tab e1 t1
            code2 <- transExp tab e2 t2
            return (code1 ++ code2 ++ [OPER op dest t1 t2])
transExp tab (RelOp op e1 e2) dest
        = do    l1 <- newLabel
                l2 <- newLabel
                code <- transCond tab (RelOp op e1 e2) l1 l2
                return ([MOVEI dest 0] ++ code ++ [LABEL l1, MOVEI dest 1] ++ [LABEL l2])
transExp tab (Bool b) dest
      | b = do return [MOVEI dest 1]
      | otherwise = do return [MOVEI dest 0]
transExp tab (UnOp NOT expr) dest
    = do    l1 <- newLabel
            l2 <- newLabel
            code <- transCond tab (UnOp NOT expr) l1 l2
            return ([MOVEI dest 0] ++ code ++ [LABEL l1, MOVEI dest 1] ++ [LABEL l2])

transExp tab (Func id (CompoundExp expr)) dest = do
    (code, temps) <- transExps tab expr
    return (code ++ [CALL dest id temps])


-- this is weird thing ngl
transExps :: Table -> [Exp] -> State Count ([Instr], [Temp])
transExps tab args = worker args
    where
    worker [] = return ([], [])
    worker (expr:exps)
      = do temp <- newTemp
           code <- transExp tab expr temp
           (code', temps) <- worker exps
           return (code++code', temp:temps)

----------------------------Trans Stm ----------------------------------------
transStm :: Table -> Label -> Stm -> State Count [Instr]
transStm tab _ (AssignStm (Id s) e) = case Map.lookup s tab of
    Nothing -> error "Undefined variable"
    Just dest -> do temp <- newTemp
                    code <- transExp tab e temp
                    return (code ++ [MOVE dest temp])

transStm tab blabel (CompoundStm stms)  = do
    list <- mapM (transStm tab blabel) stms
    return (concat list)

transStm tab blabel (IfStm expr stm) = do
    lt <- newLabel
    cont <- newLabel
    codec <- transCond tab expr lt cont
    codet <- transStm tab blabel stm
    return (codec ++ [LABEL lt] ++ codet ++ [LABEL cont])

transStm tab blabel (IfElseStm expr s1 s2) =do
    l1 <- newLabel
    l2 <- newLabel
    l3 <- newLabel
    code1 <- transCond tab expr l1 l2
    code2 <- transStm tab blabel s1
    code3 <- transStm tab blabel s2
    return (code1 ++ [LABEL l1] ++ code2 ++ [LABEL l3] ++ [LABEL l2] ++ code3 ++ [LABEL l3])

transStm tab blabel (WhileStm expr stm) =do
    l1 <- newLabel
    l2 <- newLabel
    l3 <- newLabel
    code1 <- transCond tab expr l1 l2
    code2 <- transStm tab l3 stm
    return ([LABEL l1] ++ code1 ++ [LABEL l2] ++ code2
                               ++ [JUMP l1, LABEL l3])

transStm tab blabel (ForStm (AssignStm (Id s) e) expr stm) = case Map.lookup s tab of
       Nothing -> error "invalid variable"
       Just temp -> do t1 <- newTemp
                       code1 <- transExp tab e temp
                       code2 <- transExp tab expr t1
                       l1 <- newLabel
                       l2 <- newLabel
                       l3 <- newLabel
                       code3 <- transStm tab l3 stm
                       return ( code1 ++ code2 ++ [LABEL l1, COND temp LESS t1 l2 l3, LABEL l2]
                               ++ code3 ++ [OPERI PLUS temp temp 1, JUMP l1, LABEL l3])

transStm tab blabel (BreakStm) = do return [JUMP blabel]

transStm tab blabel (ProcStm id (CompoundExp exps)) = do
    te <- newTemp
    tf <- newTemp
    (codeE,tempE) <- transExps tab exps
    return (codeE ++ [CALL id tf tempE])

-------------------------- Translate Condition ------------------------
transCond :: Table -> Exp -> Label -> Label -> State Count [Instr]
transCond _ (Bool True) lt _ = do return [JUMP lt]
transCond _ (Bool False) _ lf = do return [JUMP lf]
transCond tab (UnOp NOT cond) lt lf = transCond tab cond lf lt
transCond tab (BinOp AND c1 c2) lt lf= do
    l2 <- newLabel
    code1 <- transCond tab c1 l2 lf
    code2 <- transCond tab c2 lt lf
    return (code1 ++ [LABEL l2] ++ code2)
transCond tab (BinOp OR c1 c2) lt lf = do
    l2 <- newLabel
    code1 <- transCond tab c1 lt l2
    code2 <- transCond tab c2 lt lf
    return (code1 ++ [LABEL l2] ++ code2)
transCond tab (RelOp relop e1 e2) lt lf = do
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExp tab e1 t1
    code2 <- transExp tab e2 t2
    return (code1 ++ code2 ++ [COND t1 relop t2 lt lf])
transCond tab expr lt lf = do
    t <- newTemp
    code <- transExp tab expr t
    return ( code ++ [CONDI t DIFF 0 lt lf])
