module Main where
import DataTypes
import Lexer
import Parser
import TypeCheck
--alexScanTokens :: String -> [Tokens]


main = do
    txt <- getContents
    putStrLn $ show $ parse $ alexScanTokens txt
    putStrLn $ show $ checkProg $ parse $ alexScanTokens txt
