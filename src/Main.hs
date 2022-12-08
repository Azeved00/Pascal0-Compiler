module Main where
import DataTypes
import Lexer
import Parser
import TypeCheck
import Generator
--alexScanTokens :: String -> [Tokens]


main = do
    txt <- getContents
    --putStrLn $ show $ alexScanTokens txt
    --putStrLn $ show $ parse $ alexScanTokens txt
    --putStrLn $ show $ checkProg $ parse $ alexScanTokens txt
    putStrLn $ show $ generate $ parse $ alexScanTokens txt
