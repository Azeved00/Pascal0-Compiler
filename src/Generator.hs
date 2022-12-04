import DataTypes

import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

------------------------auxiliar functions---------------------------------
type Count = (Int,Int) 

newTemp :: State Count Temp
newTemp = do (t,l)<-get; put (t+1,l); return ("t"++show t)

popTemp :: Int -> State Count ()
popTemp k =  modify (\(t,l) -> (t-k,l))

newLabel :: State Count Label 
newLabel = do (t,l)<-get; put (t,l+1); return ("L"++show l)




-------------------- Trans Exp-------------------------------------------
transExp :: Exp -> Table -> Temp -> State Count [Instr]
transExp (Num n) tab dest = [MOVEI dest n]
transExp (Id s) tab dest = case Map.lookup s tab of
    Just temp -> return [MOVE dest t]
    Nothing -> error "invalid variable" 
transExp (BinOp op e1 e2) tab dest 
    = do    t1 <- newTemp
            t2 <- newTemp
            code1 <- transExp e1 tab t1
            code2 <- transExp e2 tab t2
            return code1 ++ code2 ++ [OPER op dest t1 t2]
transExp (Bool b) tab dest
      | b = [MOVEI dest 1]
      | otherwise = [MOVEI dest 0]
transExp (RelOp op e1 e2) tab dest 
    = do    l1 <- newLabel
            l2 <- newLabel
            cond <- RelOp op e1 e2
            code <- transCond cond l1 l2 tab
            return [MOVEI dest 0] ++ code ++ [LABEL l1, MOVEI dest 1] ++ [Label l2]
transExp (UnOp NOT expr) tab dest 
    = do    l1 <- newLabel
            l2 <- newLabel
            cond <- UnOp NOT expr
            code <- transCond cond l1 l2 tab
            return [MOVEI dest 0] ++ code ++ [LABEL l1, MOVEI dest 1] ++ [Label l2]

-- this is weird thing ngl
transExps :: Exp -> Table -> ([Instr], [Temp])
transExp (Func id expr) tab dest = code ++ [CALL dest id tl]
                                 where (code, tl) = transExps expr tab

transExps expr tab = (transExp expr tab temp, [temp])
                   where temp = newTemp()



----------------------------Trans Stm ----------------------------------------
transStm :: Stm -> Table -> State Count [Instr]
transStm (AssignStm (Id s) e) tab = case Map.lookup s tab of
    Nothing -> error "Undefined variable"
    Just dest -> do temp <- newTemp
                    code <- transExp e tab temp
                    return (code ++ [MOVE dest temp])
transStm (CompoundStm []) tab = []
transStm (CompoundStm (s1:s2)) tab  = transStm s1 tab ++ transStm s2 tab
transStm (IfStm expr stm) = 
    do  lt <- newLabel
        cont <- newLabel
        codec <- transCond expr lt tab
        codet <- transStm stm tab
        return codec ++ [LABEL lt] ++ codet ++ [LABEL cont]
transStm (IfElseStm expr s1 s2) tab = 
    do  l1 <- newLabel
        l2 <- newLabel
        l3 <- newLabel
        code1 <- transCond expr l1 l2 tab
        code2 <- transStm s1 tab
        code3 <- transStm s2 tab
        return (code1 ++ [LABEL l1] ++ code2 ++ [LABEL l3] ++ [LABEL l2] ++ code3 ++ [LABEL l3])
transStm (WhileStm expr stm) = 
    do  l1 <- newLabel
        l2 <- newLabel
        l3 <- newLabel
        code1 <- transCond expr l1 l2 tab
        code2 <- transStm stm tab
        return ([LABEL l1] ++ code1 ++ [LABEL l2] ++ code2
                               ++ [JUMP l1, LABEL l3])


-------------------------- Translate Condition ------------------------
transCond :: Exp -> Label -> Label -> Table -> State Count [Instr]
transCond (Bool True) lt _ _ = [JUMP lt]
transCond (Bool False) _ lf _ = [JUMP lf]
transCond (UnOp Not cond) lt lf tab = transCond cond lf lt tab
transCond (BinOp And c1 c2) lt lf tab = do
    l2 <- newLabel
    code1 <- transCond c1 l2 lf tab 
    code2 <- transCond c2 lt lf tab
    return (code1 ++ [Label l2] ++ code2)
transCond (BinOp Or c1 c2) lt lf tab = do
    l2 <- newLabel
    code1 <- transCond c1 lt l2 tab
    code2 <- transCond c2 lt lf tab
    return (code1 ++ [Label l2] ++ code2)
transCond (BinOp relop e1 e2) lt lf tab = do 
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExp e1 tab t1
    code2 <- transExp e2 tab t2
    return (code1 ++ code2 ++ [COND (BinOp relop e1 e2) lt lf])
transCond expr lt lf tab = do
    t <- newTemp
    code <- transExp expr tab t
    return ( code ++ [COND (BinOp DIFF t 0) lt lf])
