module Lib
    ( someFunc
    , factorial
    , fibonacci
    , factorialTail
    , applyTwice
    , multiplyBy
    , myMap
    , myFilter
    , myFoldl
    , bubbleSort
    , quickSort
    , processNumbers
    , squareAll
    ) where

-- 재귀 함수 예제
-- 팩토리얼 계산
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 피보나치 수열
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- 꼬리 재귀를 사용한 팩토리얼
factorialTail :: Int -> Int
factorialTail n = factorialHelper n 1
  where
    factorialHelper 0 acc = acc
    factorialHelper n acc = factorialHelper (n - 1) (n * acc)

-- 고차 함수 예제
-- 함수를 인자로 받는 함수
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- 함수를 반환하는 함수
multiplyBy :: Int -> (Int -> Int)
multiplyBy n = \x -> x * n

-- map 함수 구현
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- filter 함수 구현
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x = x : myFilter p xs
    | otherwise = myFilter p xs

-- fold 함수 구현
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- 정렬 알고리즘
-- 버블 정렬
bubbleSort :: [Int] -> [Int]
bubbleSort [] = []
bubbleSort xs = bubbleSort (init sorted) ++ [last sorted]
  where
    sorted = bubblePass xs
    bubblePass [] = []
    bubblePass [x] = [x]
    bubblePass (x:y:xs)
        | x > y = y : bubblePass (x:xs)
        | otherwise = x : bubblePass (y:xs)

-- 퀵 정렬
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  where
    smaller = filter (<= x) xs
    larger = filter (> x) xs

-- 함수 합성 예제
-- 여러 함수를 합성하여 파이프라인 만들기
processNumbers :: [Int] -> [Int]
processNumbers = myMap (* 2) . myFilter even . myMap (+ 1)

-- Lambda 표현식 예제
-- 익명 함수 사용
squareAll :: [Int] -> [Int]
squareAll xs = myMap (\x -> x * x) xs

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 재귀와 고차 함수 예제 ==="
    putStrLn ""
    
    -- 재귀 함수 예제
    putStrLn "재귀 함수 예제:"
    putStrLn $ "팩토리얼 5: " ++ show (factorial 5)
    putStrLn $ "피보나치 10: " ++ show (fibonacci 10)
    putStrLn $ "꼬리 재귀 팩토리얼 5: " ++ show (factorialTail 5)
    putStrLn ""
    
    -- 고차 함수 예제
    putStrLn "고차 함수 예제:"
    putStrLn $ "applyTwice (*2) 3: " ++ show (applyTwice (*2) 3)
    putStrLn $ "multiplyBy 5 4: " ++ show (multiplyBy 5 4)
    putStrLn ""
    
    -- 리스트 고차 함수 예제
    putStrLn "리스트 고차 함수 예제:"
    putStrLn $ "myMap (*2) [1,2,3,4,5]: " ++ show (myMap (*2) [1,2,3,4,5])
    putStrLn $ "myFilter even [1,2,3,4,5,6,7,8,9,10]: " ++ show (myFilter even [1,2,3,4,5,6,7,8,9,10])
    putStrLn $ "myFoldl (+) 0 [1,2,3,4,5]: " ++ show (myFoldl (+) 0 [1,2,3,4,5])
    putStrLn ""
    
    -- 정렬 알고리즘 예제
    putStrLn "정렬 알고리즘 예제:"
    putStrLn $ "버블 정렬 [3,1,4,1,5,9,2,6]: " ++ show (bubbleSort [3,1,4,1,5,9,2,6])
    putStrLn $ "퀵 정렬 [3,1,4,1,5,9,2,6]: " ++ show (quickSort [3,1,4,1,5,9,2,6])
    putStrLn ""
    
    -- 함수 합성 예제
    putStrLn "함수 합성 예제:"
    putStrLn $ "processNumbers [1,2,3,4,5,6,7,8,9,10]: " ++ show (processNumbers [1,2,3,4,5,6,7,8,9,10])
    putStrLn $ "squareAll [1,2,3,4,5]: " ++ show (squareAll [1,2,3,4,5])
