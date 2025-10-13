module Main where

import Lexer
import Parser
import System.Environment (getArgs)

-- sudo apt update
-- sudo apt install happy alex
-- alex Lexer.x       # Lexer.hs 생성
-- happy Parser.y     # Parser.hs 생성
-- ghc Main.hs Lexer.hs Parser.hs -package array
-- ./Main             # 또는 ./Main "1 + 2 * 3"


main :: IO ()
main = do
  args <- getArgs
  let input = if null args then "1 + 2 * 3" else head args
  let tokens = alexScanTokens input
  print $ parseExpr tokens
