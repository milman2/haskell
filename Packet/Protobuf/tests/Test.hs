{-# LANGUAGE OverloadedStrings #-}

module Main where

import ParserTest
import CodeGenTest
import SerializeTest

main :: IO ()
main = do
    putStrLn "=== Protobuf DSL Test Suite ==="
    putStrLn ""
    
    putStrLn "1. Running Parser Tests..."
    runAllTests
    
    putStrLn "\n2. Running Code Generation Tests..."
    runAllTests
    
    putStrLn "\n3. Running Serialization Tests..."
    runAllTests
    
    putStrLn "\n=== All Tests Completed Successfully! ==="
