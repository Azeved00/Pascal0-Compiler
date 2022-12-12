module MachineGenerator where

import DataTypes

-----------------------------Funcoes de Auxiliares-----------------------------------
writeLine :: Bool -> String -> String
writeLine tab msg = if (tab) then ("    " ++ msg ++ "\n") else (msg ++ "\n")

genOp :: Op -> String
genOp _ = "(op)"

-----------------------------Geracao de codigo maquina--------------------------------
genMachineCode :: ICode -> String
genMachineCode (defs,instrs) = 
    (writeLine False ".data")++(genData defs) ++ 
    (writeLine False ".text")++(genInstr instrs)


genData :: [Def] -> String
genData [] = ""
genData ((DARRAY label size):xs) = (writeLine True ((show label)) ++ ": .space " ++ (show size)) 
genData d = error ("Error: Can't parse Definition" ++ show d)

genInstr :: [Instr] -> String
genInstr [] = ""
genInstr ((LABEL l):xs) = (writeLine False (l++":")) ++ (genInstr xs)
genInstr ((JUMP l):xs) = (writeLine True ("j " ++ l)) ++ (genInstr xs)
genInstr ((MOVE t1 t2):xs) = (writeLine True ("move " ++ t1 ++ " " ++ t2)) ++ (genInstr xs)
genInstr ((MOVEI t1 i):xs) = (writeLine True ("li " ++ t1 ++ " " ++ (show i))) ++ (genInstr xs)
genInstr (x:xs) = (writeLine True (show x)) ++ (genInstr xs)  
--error ("Error: Can't parse Instruction -> " ++ show i)

{-
data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | MOVES Temp Label
           | OPER Op Temp Temp Temp
           | OPERI Op Temp Temp Int
           | LABEL Label
           | JUMP Label
           | COND Temp Op Temp Label Label
           | CONDI Temp Op Int Label Label
           | CALL Temp Label [Temp]
           | LOAD Temp Int Temp
           | SAVE Temp Int Temp
           | RETURN Temp
-}
