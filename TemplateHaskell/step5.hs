{-# LANGUAGE TemplateHaskell #-}

module Main where
import TH

{-
# 컴파일 및 실행:
ghc --make -package template-haskell step5.hs
./step5

# deriveShow는 컴파일 타임에 Show 인스턴스를 자동 생성합니다.
# 메타프로그래밍을 통해 보일러플레이트 코드를 줄일 수 있습니다.
# deriveShowSmart는 각 생성자를 구별해서 출력합니다.
-}

data Color = Red | Green | Blue

$(deriveShow ''Color)

main :: IO ()
main = print Red
