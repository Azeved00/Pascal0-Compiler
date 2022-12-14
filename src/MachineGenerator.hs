module MachineGenerator where

import DataTypes

-----------------------------Funcoes de Auxiliares-----------------------------------
writeLine :: Bool -> String -> String
writeLine tab msg = if (tab) then ("    " ++ msg ++ "\n") else (msg ++ "\n")

writeInstr:: String -> [String] -> String
writeInstr cmd params = writeLine True (cmd ++ (foldl (\a b -> a ++ " " ++ b) " " params))

takeThings :: String -> String
takeThings a = init (tail a)

saveS ::String
saveS = 
    (writeInstr "la" ["$v0"])++
    concat [writeInstr "sw" ["$s"++show a , (show (a*4)) ++"($v0)"] |  a <- [0..7]]


getS :: String
getS  = 
    (writeInstr "la" ["$v0"])++
    concat [writeInstr "lw" ["$s"++show a , (show (a*4)) ++ "($v0)"] |  a <- [0..7]]
-----------------------------Geracao de codigo maquina--------------------------------
genMachineCode :: ICode -> String
genMachineCode (defs,instrs) = 
    (writeLine False ".data")++
    (genData defs) ++ 
    (writeLine False ".text")++
    (writeInstr "j" ["Main"])++
    (genInstr instrs)

----------------GENERATE DATA------------------------
genData :: [Def] -> String
genData [] = ""
genData ((DARRAY label size):xs) = 
    "    " ++ (label) ++ ": .space " ++ (show (size*4)) ++ "\n" ++ (genData xs)
genData ((DSTRING label str):xs) = 
    "    " ++ (label) ++ ": .asciiz " ++ ( "\""++ (takeThings str) ++ "\"")++ " \n" ++ (genData xs) 


-----------------GENERATE INSTRUCTIONS-------------
genInstr :: [Instr] -> String
genInstr [] = ""

---------------------OPTIMIZATIONS--------------------
genInstr((MOVE t1 t2):(MOVE t3 t4):xs)
    | t1 == t4  = (writeInstr "move" [t3,t2]) ++ genInstr xs
    | otherwise = (writeInstr "move" [t1,t2]) ++ genInstr ((MOVE t3 t4) : xs)

genInstr((MOVES t1 t2):(MOVE t3 t4):xs)
    | t1 == t4  = (writeInstr "la" [t3,t2]) ++ genInstr xs
    | otherwise = (writeInstr "la" [t1,t2]) ++ genInstr ((MOVE t3 t4) : xs)

genInstr((MOVEI t1 i):(MOVE t3 t4):xs)
    | t1 == t4  = (writeInstr "li" [t3,show i]) ++ genInstr xs
    | otherwise = (writeInstr "li" [t1,show i]) ++ genInstr ((MOVE t3 t4) : xs)

--------------------SPECIFIC FUNCTIONS-----------------
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

----------------FUNCTIONS AND PROCEDURES-----------
genInstr ((CALLP id _):xs) = 
    (writeInstr "jal" [id]) ++
    (genInstr xs)

genInstr ((CALLF dest id _):xs) =
    (writeInstr "jal" [id]) ++
    (writeInstr "move" [dest,"$v0"]) ++
    (genInstr xs)

genInstr ((RETURN v):xs) = 
    (writeInstr "move" ["$v0",v]) ++
    (writeInstr "jr" ["$ra"]) ++
    (genInstr xs)
----------------------BASIC JUMPS-------------------
genInstr ((LABEL l):xs)     = (writeLine False (l++":")) ++ (genInstr xs)
genInstr ((JUMP l):xs)      = (writeInstr "j" [l]) ++ (genInstr xs)

--------------------MOVE AND OPERATIONS-------------
genInstr ((MOVE t1 t2):xs)  = (writeInstr "move"  [t1,t2]) ++ (genInstr xs)
genInstr ((MOVEI t1 i):xs)  = (writeInstr "li" [t1,show i]) ++ (genInstr xs)
genInstr ((MOVES t1 i):xs)  = (writeInstr "la" [t1,i]) ++ (genInstr xs)
genInstr ((SAVE dest i src):xs)      = (writeInstr "sw" [dest,(show i)++"("++ src ++")"]) ++ (genInstr xs)
genInstr ((LOAD src i dest):xs)      = (writeInstr "lw" [dest,(show i)++"("++ src ++")"]) ++ (genInstr xs)

genInstr ((OPER OR dest t1 t2):xs) = (writeInstr "or" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER AND dest t1 t2):xs) = (writeInstr "and" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER PLUS dest t1 t2):xs) = (writeInstr "add" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPERI PLUS dest t1 i):xs) = (writeInstr "addi" [dest,t1,show i]) ++ (genInstr xs)
genInstr ((OPER MINUS dest t1 t2):xs) = (writeInstr "sub" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER MULT dest t1 t2):xs) = (writeInstr "mul" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER DIV dest t1 t2):xs) = (writeInstr "div" [dest,t1,t2]) ++ (genInstr xs)
genInstr ((OPER MOD dest t1 t2):xs) = 
    (writeInstr "div" [t1,t2]) ++ 
    (writeInstr "mfhi" [dest]) ++ (genInstr xs)

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

