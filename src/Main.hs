module Main where
import DataTypes
import Lexer
import Parser
import TypeCheck
import Generator
import MachineGenerator
--alexScanTokens :: String -> [Tokens]


main = do
    txt <- getContents
    let lexer = alexScanTokens txt
        pars  = parse $ lexer
        typ   = checkProg $ pars
        (_data, _text)  = generate $ pars
        mips = genMachineCode (_data, _text)
    --putStrLn $ show $ lexer
    --putStrLn $ show $ pars
    --putStrLn $ show $ typ
    if typ then
        --do putStrLn (".data:\n" ++ (show _data))
        --   putStrLn (".text:\n" ++ (show _text))
        putStrLn mips
    else return ()
