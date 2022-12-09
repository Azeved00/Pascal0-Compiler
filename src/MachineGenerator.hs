module MachineGenerator where

import DataTypes

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

type ICode = ([Def], [Instr])

-----------------------------Funcoes de Auxiliares-----------------------------------
writeLine :: Bool -> String -> String
writeLine tab msg = if (tab) then ("\t" ++ msg ++ "\n") else (msg ++ "\n")


-----------------------------Geracao de codigo maquina--------------------------------
genMachineCode :: ICode -> String
genMachineCode (defs,instrs) = (writeLine False ".data")++(genData defs) ++ (writeLine False ".text")++(genInstr instrs)


genData :: [Def] -> String
genData [] = ""
genData ((DARRAY label size):xs) = writeLine true ((show label)++": .space "++(show size)) 
genData d = error ("Error: Can't parse Definition" ++ show d)

genInstr :: [Instr] -> String
genInstr [] = ""
genInstr i = error ("Error: Can't parse Instruction -> " ++ show i)


