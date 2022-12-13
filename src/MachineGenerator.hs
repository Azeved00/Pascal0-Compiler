module MachineGenerator where

import DataTypes

-----------------------------Funcoes de Auxiliares-----------------------------------
writeLine :: Bool -> String -> String
writeLine tab msg = if (tab) then ("    " ++ msg ++ "\n") else (msg ++ "\n")

writeInstr:: String -> [String] -> String
writeInstr cmd params = writeLine True (cmd ++ (foldl (\a b -> a ++ " " ++ b) " " params))

-----------------------------Geracao de codigo maquina--------------------------------
genMachineCode :: ICode -> String
genMachineCode (defs,instrs) = 
    (writeLine False ".data")++(genData defs) ++ 
    (writeLine False ".text")++(genInstr instrs)


genData :: [Def] -> String
genData [] = ""
genData ((DARRAY label size):xs) = (writeLine True (show label)) ++ ": .space " ++ (show size) 
genData d = error ("Error: Can't parse Definition" ++ show d)

genInstr :: [Instr] -> String
genInstr [] = ""

genInstr ((LABEL l):xs)     = (writeLine False (l++":")) ++ (genInstr xs)
genInstr ((JUMP l):xs)      = (writeInstr "j" [l]) ++ (genInstr xs)

genInstr ((MOVE t1 t2):xs)  = (writeInstr "move"  [t1,t2]) ++ (genInstr xs)
genInstr ((MOVEI t1 i):xs)  = (writeInstr "li" [t1,show i]) ++ (genInstr xs)

genInstr ((OPER PLUS dest t1 t2):xs) = (writeInstr "add" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPERI PLUS dest t1 i):xs) = (writeInstr "addi" [dest,t1,show i]) ++ (genInstr xs)
genInstr ((OPER MINUS dest t1 t2):xs) = (writeInstr "sub" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER MULT dest t1 t2):xs) = (writeInstr "mul" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER DIV dest t1 t2):xs) = (writeInstr "div" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER MOD dest t1 t2):xs) = 
    (writeInstr "div" [t1,t2]) ++ 
    (writeInstr "mdhi" [dest]) ++ (genInstr xs)

genInstr ((COND t1 EQUAL t2 lt lf):(LABEL l1):xs) 
    | l1 == lt = (writeInstr "bne" [t1,t2,lf]) ++ cont
    | l1 == lf = (writeInstr "beq" [t1,t2,lt]) ++ cont
    | otherwise = error ("Error : Condition has no where to go")
    where cont = genInstr xs
genInstr ((COND t1 DIFF t2 lt lf):(LABEL l1):xs)
    | l1 == lt = (writeInstr "beq" [t1,t2,lf]) ++ cont
    | l1 == lf = (writeInstr "bne" [t1,t2,lt]) ++ cont
    | otherwise = error ("Error : Condition has no where to go")
    where cont = genInstr xs
genInstr ((COND t1 GREAT t2 lt lf):(LABEL l1):xs)
    | l1 == lt = (writeInstr "ble" [t1,t2,lf]) ++ cont
    | l1 == lf = (writeInstr "bgt" [t1,t2,lt]) ++ cont
    | otherwise = error ("Error : Condition has no where to go")
    where cont = genInstr xs
genInstr ((COND t1 GEQUAL t2 lt lf):(LABEL l1):xs) 
    | l1 == lt = (writeInstr "blt" [t1,t2,lf]) ++ cont
    | l1 == lf = (writeInstr "bge" [t1,t2,lt]) ++ cont
    | otherwise = error ("Error : Condition has no where to go")
    where cont = genInstr xs
genInstr ((COND t1 LESS t2 lt lf):(LABEL l1):xs) 
    | l1 == lt = (writeInstr "bge" [t1,t2,lf]) ++ cont
    | l1 == lf = (writeInstr "blt" [t1,t2,lt]) ++ cont
    | otherwise = error ("Error : Condition has no where to go")
    where cont = genInstr xs
genInstr ((COND t1 LEQUAL t2 lt lf):(LABEL l1):xs)
    | l1 == lt = (writeInstr "bgt" [t1,t2,lf]) ++ cont
    | l1 == lf = (writeInstr "ble" [t1,t2,lt]) ++ cont
    | otherwise = error ("Error : Condition has no where to go")
    where cont = genInstr xs

genInstr (x:xs) = (writeLine True (show x)) ++ (genInstr xs)  
--error ("Error: Can't parse Instruction -> " ++ show i)

{-
data Instr =
           | MOVES Temp Label
           | OPERI Op Temp Temp Int
           | COND Temp Op Temp Label Label
           | CONDI Temp Op Int Label Label
           | CALL Temp Label [Temp]
           | LOAD Temp Int Temp
           | SAVE Temp Int Temp
           | RETURN Temp
-}
