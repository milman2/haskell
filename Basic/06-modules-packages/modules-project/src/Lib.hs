module Lib
    ( someFunc
    , factorial
    , fibonacci
    , isPrime
    , gcd
    , lcm
    , Statistics(..)
    , calculateStats
    ) where

-- 수학 유틸리티 함수들
-- 팩토리얼 계산
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 피보나치 수열
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- 소수 판별
isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [3,5..(floor $ sqrt $ fromIntegral n)]

-- 최대공약수
gcd :: Integer -> Integer -> Integer
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

-- 최소공배수
lcm :: Integer -> Integer -> Integer
lcm a b = (a * b) `div` (gcd a b)

-- 통계 데이터 타입
data Statistics = Statistics
    { mean :: Double
    , median :: Double
    , mode :: Maybe Integer
    , standardDeviation :: Double
    } deriving (Show, Eq)

-- 통계 계산
calculateStats :: [Integer] -> Statistics
calculateStats [] = Statistics 0 0 Nothing 0
calculateStats xs = Statistics
    { mean = fromIntegral (sum xs) / fromIntegral (length xs)
    , median = calculateMedian xs
    , mode = calculateMode xs
    , standardDeviation = calculateStdDev xs
    }

calculateMedian :: [Integer] -> Double
calculateMedian xs = 
    let sorted = sort xs
        len = length sorted
    in if even len
        then fromIntegral (sorted !! (len `div` 2 - 1) + sorted !! (len `div` 2)) / 2
        else fromIntegral (sorted !! (len `div` 2))

calculateMode :: [Integer] -> Maybe Integer
calculateMode xs = 
    let counts = map (\x -> (x, length $ filter (== x) xs)) (nub xs)
        maxCount = maximum $ map snd counts
    in if maxCount == 1 then Nothing else Just (fst $ head $ filter ((== maxCount) . snd) counts)

calculateStdDev :: [Integer] -> Double
calculateStdDev xs = 
    let avg = fromIntegral (sum xs) / fromIntegral (length xs)
        variance = sum $ map (\x -> (fromIntegral x - avg) ^ 2) xs
    in sqrt (variance / fromIntegral (length xs))

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 모듈과 패키지 관리 예제 ==="
    putStrLn ""
    
    putStrLn "수학 유틸리티 함수들:"
    putStrLn $ "팩토리얼 5: " ++ show (factorial 5)
    putStrLn $ "피보나치 10: " ++ show (fibonacci 10)
    putStrLn $ "17은 소수인가? " ++ show (isPrime 17)
    putStrLn $ "15는 소수인가? " ++ show (isPrime 15)
    putStrLn $ "gcd 48 18: " ++ show (gcd 48 18)
    putStrLn $ "lcm 12 18: " ++ show (lcm 12 18)
    putStrLn ""
    
    putStrLn "통계 계산:"
    let stats = calculateStats [1,2,2,3,3,3,4,4,5]
    putStrLn $ "데이터: [1,2,2,3,3,3,4,4,5]"
    putStrLn $ "평균: " ++ show (mean stats)
    putStrLn $ "중앙값: " ++ show (median stats)
    putStrLn $ "최빈값: " ++ show (mode stats)
    putStrLn $ "표준편차: " ++ show (standardDeviation stats)
