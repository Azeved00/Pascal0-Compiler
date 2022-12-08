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
        (_data, _text)  = generate $ pars
    --putStrLn $ show $ lexer
    --putStrLn $ show $ pars
    --putStrLn $ show $ typ
    if typ then
        do putStrLn (".data:\n" ++ (show _data))
           putStrLn (".text:\n" ++ (show _text))
    else return ()
