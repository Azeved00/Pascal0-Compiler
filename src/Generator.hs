module Generator where

import DataTypes
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map



generate :: Prog -> ICode
generate p =  evalState (genInstr p) (0,0,0)

type Count = (Int,Int,Int)
type Table = Map Id Temp

genInstr :: Prog -> State Count ICode
genInstr (Program _ (Body const procs vars prog)) = do
    t1 <- loadConsts Map.empty const
    (def1, procs) <- loadProcs t1 procs const
    (t2, def2) <- loadVars t1 vars const
    (def3, list) <- transStm t2 "" prog
    return (def1++def2++def3, [JUMP "Main"] ++ procs ++ [LABEL "Main"]++list)

myconcat :: [ICode] -> ICode
myconcat [] = ([],[])
myconcat ((d,i):xs) = (d++d1, i++i1)
                    where (d1,i1) = myconcat xs

-------------- Load Stuff ------------------------------------
getConstValues :: [Const] -> Exp  -> Int
getConstValues _ (Num n) = n
getConstValues [] e = error ("Error : " ++ show e++ "is not available");
getConstValues ((id,v):xs) (Id c) =
    if (id == c) then v
    else getConstValues xs (Id c)
getConstValues _ e = error ("Error: "++ show e ++ " is not valid array definition")

loadVars :: Table -> [Var] -> [Const] -> State Count (Table, [Def])
loadVars t [] _ = return (t, [])
loadVars t ((id,TyArray _ e1 e2):vs)  consts= do
    temp <- newTemp
    (nt, def) <- loadVars (Map.insert id temp t) vs consts
    let size = (getConstValues consts e2) - (getConstValues consts e1)
    if(size > 0) then return (nt, def ++ [DARRAY id size])
    else error ( "Error: Array" ++ show id ++" has an invalid size")
loadVars t ((id,_):vs) consts  = do
    temp <- newTemp
    (nt, def) <- loadVars (Map.insert id temp t) vs consts
    return (nt, def)

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


loadProcs :: Table -> [Proc] -> [Const] -> State Count ICode
loadProcs tab [] _= return ([],[])
loadProcs tab (((Procedure l params),(vrs,stms)):xs) const = do
    ntab1   <- loadParams tab params
    (ntab2, def1)   <- loadVars ntab1 vrs const
    endl    <- newLabel
    (def2, stcode)  <- transStm ntab2 l stms
    (def3, other)   <- loadProcs tab xs const
    return (def1++def2++def3, [LABEL l] ++ stcode ++ other)

loadProcs tab (((Function id params tpe),(vrs,stms)):xs) const= do
    ntab1   <- loadParams tab params
    (ntab2, def1)   <- loadVars ntab1 ((id,tpe):vrs) const
    endl    <- newLabel
    (def2, fcode)   <- transStm ntab2 endl stms
    (def3, other)   <- loadProcs tab xs const
    return (def1++def2++def3, [LABEL id] ++ fcode ++[RETURN id] ++ other)
------------------------auxiliar functions---------------------------------

newTemp :: State Count Temp
newTemp = do (t,l,s)<-get; put (t+1,l,s); return ("t"++show t)

popTemp :: Int -> State Count ()
popTemp k =  modify (\(t,l,s) -> (t-k,l,s))

newLabel :: State Count Label
newLabel = do (t,l,s)<-get; put (t,l+1,s); return ("L"++show l)

newStr :: State Count Label
newStr = do (t,l,s)<-get; put (t,l,s+1); return ("s"++show l)

-------------------- Trans Exp-------------------------------------------
transExp :: Table -> Exp -> Temp -> State Count ICode
transExp tab (Num n) dest = do return ([],[MOVEI dest n])

transExp tab (Id s) dest = case Map.lookup s tab of
    Just temp -> return ([],[MOVE dest temp])
    Nothing -> error ("Error:" ++ show s ++ " invalid variable")

transExp tab (Str s) dest = do ls <- newStr
                               return ([DSTRING ls s],[MOVES dest ls])

transExp tab (Array s expr) dest = case Map.lookup s tab of
    Just temp -> do t1 <- newTemp
                    (def, code) <- transExp tab expr t1
                    return (def, [MOVES temp s] ++ code ++ [OPER PLUS temp temp t1, LOAD temp 0 dest])
    Nothing -> error ("Error:" ++ show s ++ " invalid variable")

transExp tab (BinOp op e1 e2) dest = do
    t1 <- newTemp
    t2 <- newTemp
    (def1, code1) <- transExp tab e1 t1
    (def2, code2) <- transExp tab e2 t2
    return (def1++def2, code1 ++ code2 ++ [OPER op dest t1 t2])

transExp tab (RelOp op e1 e2) dest = do
    l1 <- newLabel
    l2 <- newLabel
    (def, code) <- transCond tab (RelOp op e1 e2) l1 l2
    return (def, [MOVEI dest 0] ++ code ++ [LABEL l1, MOVEI dest 1] ++ [LABEL l2])

transExp tab (Bool b) dest
      | b = do return ([],[MOVEI dest 1])
      | otherwise = do return ([],[MOVEI dest 0])

transExp tab (UnOp NOT expr) dest = do
    l1 <- newLabel
    l2 <- newLabel
    (def, code) <- transCond tab (UnOp NOT expr) l1 l2
    return (def, [MOVEI dest 0] ++ code ++ [LABEL l1, MOVEI dest 1] ++ [LABEL l2])

transExp tab (Func id (CompoundExp expr)) dest = do
    ((def, code), temps) <- transExps tab expr
    return (def, code ++ [CALL dest id temps])

transExp _ exp _ = error ("Error: Cant 't parse" ++ show exp)

-- this is weird thing ngl
transExps :: Table -> [Exp] -> State Count (ICode,[Temp])
transExps tab args = worker args
    where
    worker [] = return (([], []), [])
    worker (expr:exps)
      = do temp <- newTemp
           (def, code) <- transExp tab expr temp
           ((def', code'), temps) <- worker exps
           return ((def++def', code++code'), temp:temps)

----------------------------Trans Stm ----------------------------------------
transStm :: Table -> Label -> Stm -> State Count ICode
transStm tab _ (AssignStm (Id s) e) = case Map.lookup s tab of
    Nothing -> error "Undefined variable"
    Just dest -> do temp <- newTemp
                    (def, code) <- transExp tab e temp
                    return (def, code ++ [MOVE dest temp])

transStm tab _ (AssignStm (Array s expr) e) = case Map.lookup s tab of
    Nothing -> error "Undefined variable"
    Just dest -> do t1 <- newTemp
                    (def1, code1) <- transExp tab expr t1
                    t2 <- newTemp
                    (def2, code2) <- transExp tab e t2
                    return (def1++def2, [MOVES dest s] ++ code1 ++ [OPER PLUS dest dest t1]
                                        ++ code2 ++ [SAVE t2 0 dest])

transStm tab blabel (CompoundStm stms)  = do
    list <- mapM (transStm tab blabel) stms
    return (myconcat list)

transStm tab blabel (IfStm expr stm) = do
    lt <- newLabel
    cont <- newLabel
    (defc, codec) <- transCond tab expr lt cont
    (deft, codet) <- transStm tab blabel stm
    return (defc++deft, codec ++ [LABEL lt] ++ codet ++ [LABEL cont])

transStm tab blabel (IfElseStm expr s1 s2) =do
    l1 <- newLabel
    l2 <- newLabel
    l3 <- newLabel
    (def1, code1) <- transCond tab expr l1 l2
    (def2, code2) <- transStm tab blabel s1
    (def3, code3) <- transStm tab blabel s2
    return (def1++def2++def3, code1 ++ [LABEL l1] ++ code2 ++ [JUMP l3] ++ [LABEL l2]
                              ++ code3 ++ [LABEL l3])

transStm tab blabel (WhileStm expr stm) =do
    l1 <- newLabel
    l2 <- newLabel
    l3 <- newLabel
    (def1, code1) <- transCond tab expr l1 l2
    (def2, code2) <- transStm tab l3 stm
    return (def1++def2, [LABEL l1] ++ code1 ++ [LABEL l2] ++ code2
                               ++ [JUMP l1, LABEL l3])

transStm tab blabel (ForStm (AssignStm (Id s) e) expr stm) = case Map.lookup s tab of
       Nothing -> error "invalid variable"
       Just temp -> do t1 <- newTemp
                       (def1, code1) <- transExp tab e temp
                       (def2, code2) <- transExp tab expr t1
                       l1 <- newLabel
                       l2 <- newLabel
                       l3 <- newLabel
                       (def3, code3) <- transStm tab l3 stm
                       return (def1++def2++def3, code1 ++ code2 ++ [LABEL l1, COND temp LESS t1 l2 l3, LABEL l2]
                               ++ code3 ++ [OPERI PLUS temp temp 1, JUMP l1, LABEL l3])

transStm tab blabel (BreakStm) = do return ([], [JUMP blabel])

transStm tab blabel (ProcStm id (CompoundExp exps)) = do
    te <- newTemp
    tf <- newTemp
    ((defE, codeE),tempE) <- transExps tab exps
    return (defE, codeE ++ [CALL id tf tempE])

transStm tab blabel st = error ("Error: Can't parse statement: " ++ show st)
-------------------------- Translate Condition ------------------------
transCond :: Table -> Exp -> Label -> Label -> State Count ICode
transCond _ (Bool True) lt _ = do return ([],[JUMP lt])
transCond _ (Bool False) _ lf = do return ([],[JUMP lf])
transCond tab (UnOp NOT cond) lt lf = transCond tab cond lf lt
transCond tab (BinOp AND c1 c2) lt lf= do
    l2 <- newLabel
    (def1, code1) <- transCond tab c1 l2 lf
    (def2, code2) <- transCond tab c2 lt lf
    return (def1++def2, code1 ++ [LABEL l2] ++ code2)
transCond tab (BinOp OR c1 c2) lt lf = do
    l2 <- newLabel
    (def1, code1) <- transCond tab c1 lt l2
    (def2, code2) <- transCond tab c2 lt lf
    return (def1++def2, code1 ++ [LABEL l2] ++ code2)
transCond tab (RelOp relop e1 e2) lt lf = do
    t1 <- newTemp
    t2 <- newTemp
    (def1, code1) <- transExp tab e1 t1
    (def2, code2) <- transExp tab e2 t2
    return (def1++def2, code1 ++ code2 ++ [COND t1 relop t2 lt lf])
transCond tab expr lt lf = do
    t <- newTemp
    (def, code) <- transExp tab expr t
    return (def, code ++ [CONDI t DIFF 0 lt lf])
