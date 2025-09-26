module Main where

import HaskellPDSL
import ConfigPDSL
import QueryPDSL

main :: IO ()
main = do
    testPDSL
    putStrLn $ "=" ++ replicate 50 '='
    putStrLn ""
    testConfigPDSL
    putStrLn $ "=" ++ replicate 50 '='
    putStrLn ""
    testQueryPDSL
