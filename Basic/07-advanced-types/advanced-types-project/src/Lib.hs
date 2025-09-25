{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies #-}

module Lib
    ( someFunc
    , Vector(..)
    , VNil
    , VCons
    , append
    , index
    , vmap
    , vfoldr
    ) where

import GHC.TypeLits

-- 타입 안전한 벡터
data Vector (n :: Nat) a where
    VNil :: Vector 0 a
    VCons :: a -> Vector n a -> Vector (n + 1) a

-- 벡터 연결
append :: Vector n a -> Vector m a -> Vector (n + m) a
append VNil ys = ys
append (VCons x xs) ys = VCons x (append xs ys)

-- 벡터 맵
vmap :: (a -> b) -> Vector n a -> Vector n b
vmap _ VNil = VNil
vmap f (VCons x xs) = VCons (f x) (vmap f xs)

-- 벡터 폴드
vfoldr :: (a -> b -> b) -> b -> Vector n a -> b
vfoldr _ acc VNil = acc
vfoldr f acc (VCons x xs) = f x (vfoldr f acc xs)

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 고급 타입 시스템 예제 ==="
    putStrLn ""
    
    putStrLn "타입 안전한 벡터 예제:"
    let vec1 = VCons 1 (VCons 2 (VCons 3 VNil))
        vec2 = VCons 4 (VCons 5 VNil)
        combined = append vec1 vec2
        doubled = vmap (*2) combined
        sum = vfoldr (+) 0 combined
    
    putStrLn $ "벡터1: " ++ show vec1
    putStrLn $ "벡터2: " ++ show vec2
    putStrLn $ "합친 벡터: " ++ show combined
    putStrLn $ "2배한 벡터: " ++ show doubled
    putStrLn $ "합계: " ++ show sum
