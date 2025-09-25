module Lib
    ( someFunc
    , parallelMergeSort
    , parallelQuickSort
    , parallelMap
    , parallelFilter
    ) where

import Control.Parallel
import Control.Parallel.Strategies

-- 병렬 머지 정렬
parallelMergeSort :: Ord a => [a] -> [a]
parallelMergeSort [] = []
parallelMergeSort [x] = [x]
parallelMergeSort xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
        sortedLeft = parallelMergeSort left
        sortedRight = parallelMergeSort right
    in sortedLeft `par` sortedRight `pseq` merge sortedLeft sortedRight

-- 머지 함수
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- 병렬 퀵 정렬
parallelQuickSort :: Ord a => [a] -> [a]
parallelQuickSort [] = []
parallelQuickSort [x] = [x]
parallelQuickSort (x:xs) = 
    let (smaller, larger) = partition (<= x) xs
        sortedSmaller = parallelQuickSort smaller
        sortedLarger = parallelQuickSort larger
    in sortedSmaller `par` sortedLarger `pseq` sortedSmaller ++ [x] ++ sortedLarger

-- 전략을 사용한 병렬 처리
parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f xs = map f xs `using` parList rseq

-- 병렬 필터
parallelFilter :: (a -> Bool) -> [a] -> [a]
parallelFilter p xs = filter p xs `using` parList rseq

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 동시성과 병렬성 예제 ==="
    putStrLn ""
    
    let testData = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3,2,3,8,4,6,2,6,4,3,3,8,3,2,7,9,5,0,2,8,8,4,1,9,7,1,6,9,3,9,9,3,7,5,1,0,5,8,2,0,9,7,4,9,4,4,5,9,2,3,0,7,8,1,6,4,0,6,2,8,6,2,0,8,9,9,8,6,2,8,0,3,4,8,2,5,3,4,2,1,1,7,0,6,7]
    
    putStrLn "병렬 정렬 알고리즘 예제:"
    putStrLn $ "원본 데이터 (처음 20개): " ++ show (take 20 testData)
    putStrLn $ "병렬 머지 정렬 (처음 20개): " ++ show (take 20 $ parallelMergeSort testData)
    putStrLn $ "병렬 퀵 정렬 (처음 20개): " ++ show (take 20 $ parallelQuickSort testData)
    putStrLn ""
    
    putStrLn "병렬 처리 예제:"
    putStrLn $ "병렬 맵 (*2) [1..10]: " ++ show (parallelMap (*2) [1..10])
    putStrLn $ "병렬 필터 even [1..20]: " ++ show (parallelFilter even [1..20])
