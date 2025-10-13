{-# LANGUAGE TemplateHaskell #-}

module Main where
import TH

{-
# 컴파일 및 실행:
ghc --make -package template-haskell step3.hs
./step3

# reify는 컴파일 타임에 타입 정보를 조회합니다.
# 같은 모듈에서 정의한 타입은 조회할 수 없으므로 (stage restriction)
# Prelude에 정의된 타입들을 사용합니다.
-}

main :: IO ()
main = do
  putStrLn "=== Bool 타입 정보 ==="
  $(inspectType ''Bool)
  putStrLn "\n=== Maybe 타입 정보 ==="
  $(inspectType ''Maybe)
