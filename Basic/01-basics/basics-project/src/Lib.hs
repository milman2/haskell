module Lib
    ( someFunc
    , add
    , multiply
    , intToFloat
    , isEven
    , maxInt
    , average
    , stringLength
    , absoluteValue
    ) where

-- 기본 타입 예제
-- Int 타입
age :: Int
age = 25

-- Float 타입
piValue :: Float
piValue = 3.14159

-- Bool 타입
isAdult :: Bool
isAdult = age >= 18

-- Char 타입
firstLetter :: Char
firstLetter = 'H'

-- String 타입
greeting :: String
greeting = "Hello, Haskell!"

-- 함수 정의 예제
add :: Int -> Int -> Int
add x y = x + y

multiply :: Float -> Float -> Float
multiply x y = x * y

-- 타입 변환 함수
intToFloat :: Int -> Float
intToFloat x = fromIntegral x

-- 조건부 함수
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- 연습 문제 해답들
-- 1. 두 정수의 최대값을 반환하는 함수
maxInt :: Int -> Int -> Int
maxInt x y = if x > y then x else y

-- 2. 세 개의 정수를 받아서 평균을 계산하는 함수
average :: Int -> Int -> Int -> Float
average x y z = fromIntegral (x + y + z) / 3.0

-- 3. 문자열의 길이를 계산하는 함수
stringLength :: String -> Int
stringLength str = length str

-- 4. 정수를 받아서 절댓값을 반환하는 함수
absoluteValue :: Int -> Int
absoluteValue x = if x >= 0 then x else -x

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 기본 문법과 타입 시스템 예제 ==="
    putStrLn ""
    
    -- 기본 타입 출력
    putStrLn "기본 타입 예제:"
    putStrLn $ "나이: " ++ show age
    putStrLn $ "파이 값: " ++ show piValue
    putStrLn $ "성인 여부: " ++ show isAdult
    putStrLn $ "첫 글자: " ++ show firstLetter
    putStrLn $ "인사말: " ++ greeting
    putStrLn ""
    
    -- 함수 사용 예제
    putStrLn "함수 사용 예제:"
    putStrLn $ "5 + 3 = " ++ show (add 5 3)
    putStrLn $ "2.5 * 4.0 = " ++ show (multiply 2.5 4.0)
    putStrLn $ "정수 10을 Float로 변환: " ++ show (intToFloat 10)
    putStrLn $ "8은 짝수인가? " ++ show (isEven 8)
    putStrLn $ "7은 짝수인가? " ++ show (isEven 7)
    putStrLn ""
    
    -- 연습 문제 해답
    putStrLn "연습 문제 해답:"
    putStrLn $ "10과 20 중 최댓값: " ++ show (maxInt 10 20)
    putStrLn $ "1, 2, 3의 평균: " ++ show (average 1 2 3)
    putStrLn $ "'Hello'의 길이: " ++ show (stringLength "Hello")
    putStrLn $ "-5의 절댓값: " ++ show (absoluteValue (-5))
    putStrLn $ "5의 절댓값: " ++ show (absoluteValue 5)
