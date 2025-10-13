{-# LANGUAGE TemplateHaskell #-}

module Main where
import TH

{-
# 컴파일 및 실행 (Template Haskell은 runghc가 아닌 ghc --make 사용):
ghc --make -package template-haskell step1.hs
./step1

# 참고: runghc는 Template Haskell 다중 모듈 프로젝트에서 제대로 작동하지 않습니다.
-}
main :: IO ()
main = print $ $(squareExpr) 5
