module Generator where

import DataTypes
import           Control.Monad.State
import           Data.Map (Map)
import qualified Data.Map as Map

generate :: Prog -> [Instr]
generate (Program _ (Body _ _ _ prog)) = list
                                       where list = evalState (transStm Map.empty prog) (0,0)

------------------------auxiliar functions---------------------------------
type Count = (Int,Int)
type Table = Map Id Temp

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
    Nothing -> error "invalid variable"

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

transExp tab (Func id (CompoundExp expr)) dest
  = case Map.lookup id tab of
    Nothing  -> error "undefined function"
    Just flabel -> do (code, temps) <- transExps tab expr
                      return (code ++ [CALL dest flabel temps])


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
transStm :: Table -> Stm -> State Count [Instr]
transStm tab (AssignStm (Id s) e) = case Map.lookup s tab of
    Nothing -> error "Undefined variable"
    Just dest -> do temp <- newTemp
                    code <- transExp tab e temp
                    return (code ++ [MOVE dest temp])

transStm tab (CompoundStm stms)  = do
  list <- mapM (transStm tab) stms
  return (concat list)

transStm tab (IfElseStm expr s1 s2) =
    do  l1 <- newLabel
        l2 <- newLabel
        l3 <- newLabel
        code1 <- transCond tab expr l1 l2
        code2 <- transStm tab s1
        code3 <- transStm tab s2
        return (code1 ++ [LABEL l1] ++ code2 ++ [LABEL l3] ++ [LABEL l2] ++ code3 ++ [LABEL l3])
transStm tab (WhileStm expr stm) =
    do  l1 <- newLabel
        l2 <- newLabel
        l3 <- newLabel
        code1 <- transCond tab expr l1 l2
        code2 <- transStm tab stm
        return ([LABEL l1] ++ code1 ++ [LABEL l2] ++ code2
                               ++ [JUMP l1, LABEL l3])

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
transCond tab (BinOp relop e1 e2) lt lf = do
    t1 <- newTemp
    t2 <- newTemp
    code1 <- transExp tab e1 t1
    code2 <- transExp tab e2 t2
    return (code1 ++ code2 ++ [COND (BinOp relop e1 e2) lt lf])
transCond tab expr lt lf = do
    t <- newTemp
    code <- transExp tab expr t
    return ( code ++ [COND (BinOp DIFF (Id t) (Num 0)) lt lf])
