module Main where

import SimpleMath
import QueryDSL
import ConfigDSL

main :: IO ()
main = do
    testMath
    putStrLn $ "=" ++ replicate 50 '='
    putStrLn ""
    testQuery
    putStrLn $ "=" ++ replicate 50 '='
    putStrLn ""
    testConfig
