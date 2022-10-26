module Main where
import Lexer
import Data.Char
--alexScanTokens :: String -> [Tokens]


main = do
    txt <- getContents
    print (alexScanTokens (map toLower txt))
