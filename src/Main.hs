module Main where
import DataTypes
import Lexer
import Parser
import TypeCheck
import Generator
--alexScanTokens :: String -> [Tokens]


main = do
    txt <- getContents
    let lexer = alexScanTokens txt
        pars  = parse $ lexer
        typ   = checkProg $ pars
        code  = generate $ pars
    --putStrLn $ show $ lexer
    --putStrLn $ show $ pars
    --putStrLn $ show $ typ
    if typ then putStrLn $ show $ code else return ()
