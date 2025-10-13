{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where
import TH

{-
# 컴파일 및 실행:
ghc --make -package template-haskell step4.hs
./step4

# QuasiQuoter는 사용자 정의 문법 확장을 제공합니다.
# [myQuote|...|] 안의 문자열을 컴파일 타임에 처리합니다.
-}

main :: IO ()
main = putStrLn [myQuote|Hello Template Haskell!|]

{-
활용 사례:
    정규 표현식: [regex|\d+|]
    SQL 쿼리: [sql|SELECT * FROM users|]
    JSON: [json|{"name": "John"}|]
    HTML: [html|<div>Hello</div>|]
-}