module Main where
import DataTypes
import Lexer
import Parser
import TypeCheck
import Data.Char
--alexScanTokens :: String -> [Tokens]


main = do
    txt <- getContents
    print (parse $ alexScanTokens $ map toLower txt )
    print (checkProg $ parse $ alexScanTokens $ map toLower txt ) 
