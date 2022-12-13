module MachineGenerator where

import DataTypes

-----------------------------Funcoes de Auxiliares-----------------------------------
writeLine :: Bool -> String -> String
writeLine tab msg = if (tab) then ("    " ++ msg ++ "\n") else (msg ++ "\n")

writeInstr:: String -> [String] -> String
writeInstr cmd params = writeLine True (cmd ++ (foldl (\a b -> a ++ " " ++ b) " " params))

takeThings :: String -> String
takeThings a = init (tail a)
-----------------------------Geracao de codigo maquina--------------------------------
genMachineCode :: ICode -> String
genMachineCode (defs,instrs) = 
    (writeLine False ".data")++(genData defs) ++ 
    (writeLine False ".text")++(genInstr instrs)

----------------GENERATE DATA------------------------
genData :: [Def] -> String
genData [] = ""
genData ((DARRAY label size):xs) = 
    "    " ++ (label) ++ ": .space " ++ (show size) ++ "\n" ++ (genData xs)
genData ((DSTRING label str):xs) = 
    "    " ++ (label) ++ ": .asciiz " ++ ( "\""++ (takeThings str) ++ "\"")++ " \n" ++ (genData xs) 


-----------------GENERATE INSTRUCTIONS-------------
genInstr :: [Instr] -> String
genInstr [] = ""

--------------------SPECIFIC CASES -----------------
genInstr ((CALLP "writeint" [i]):xs) = 
    (writeInstr "li" ["$v0","1"]) ++
    (writeInstr "move" ["$a0",i]) ++
    (writeInstr "syscall" []) ++
    (genInstr xs)

genInstr ((CALLP "writestr" [i]):xs) = 
    (writeInstr "li" ["$v0","4"]) ++
    (writeInstr "move" ["$a0",i]) ++
    (writeInstr "syscall" []) ++
    (genInstr xs)


genInstr ((CALLF dest "readint" _):xs) = 
    (writeInstr "li" ["$v0","5"]) ++
    (writeInstr "syscall" []) ++
    (writeInstr "move" [dest,"$v0"])++
    (genInstr xs)
----------------------BASIC JUMPS-------------------
genInstr ((LABEL l):xs)     = (writeLine False (l++":")) ++ (genInstr xs)
genInstr ((JUMP l):xs)      = (writeInstr "j" [l]) ++ (genInstr xs)

--------------------MOVE AND OPERATIONS-------------
genInstr ((MOVE t1 t2):xs)  = (writeInstr "move"  [t1,t2]) ++ (genInstr xs)
genInstr ((MOVEI t1 i):xs)  = (writeInstr "li" [t1,show i]) ++ (genInstr xs)
genInstr ((MOVES t1 i):xs)  = (writeInstr "la" [t1,i]) ++ (genInstr xs)

genInstr ((OPER PLUS dest t1 t2):xs) = (writeInstr "add" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPERI PLUS dest t1 i):xs) = (writeInstr "addi" [dest,t1,show i]) ++ (genInstr xs)
genInstr ((OPER MINUS dest t1 t2):xs) = (writeInstr "sub" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER MULT dest t1 t2):xs) = (writeInstr "mul" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER DIV dest t1 t2):xs) = (writeInstr "div" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER MOD dest t1 t2):xs) = 
    (writeInstr "div" [t1,t2]) ++ 
    (writeInstr "mdhi" [dest]) ++ (genInstr xs)

-----------------CONDITIONS------------------------
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

