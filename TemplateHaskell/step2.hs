{-# LANGUAGE TemplateHaskell #-}

module Main where
import TH

{-
# 컴파일 및 실행:
ghc --make -package template-haskell step2.hs
./step2
-}

$(genHello)

main :: IO ()
main = hello
