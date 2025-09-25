module Lib
    ( someFunc
    , safeDivide
    , findElement
    , safeSqrt
    , safeDivideEither
    , complexMath
    , processUserInput
    , applyToMaybe
    , combineMaybes
    , increment
    ) where

import Control.Monad

-- 타입 클래스 예제
-- 커스텀 타입 정의
data Person = Person String Int deriving (Show, Eq)

-- 타입 클래스 인스턴스 구현
instance Ord Person where
    compare (Person _ age1) (Person _ age2) = compare age1 age2

-- Maybe 타입 예제
-- 안전한 나눗셈
safeDivide :: Float -> Float -> Maybe Float
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- 리스트에서 요소 찾기
findElement :: Eq a => a -> [a] -> Maybe Int
findElement _ [] = Nothing
findElement x (y:ys)
    | x == y = Just 0
    | otherwise = case findElement x ys of
        Nothing -> Nothing
        Just idx -> Just (idx + 1)

-- Either 타입 예제
-- 에러 타입 정의
data MathError = DivisionByZero | NegativeSquareRoot deriving (Show)

-- 안전한 수학 연산
safeSqrt :: Float -> Either MathError Float
safeSqrt x
    | x < 0 = Left NegativeSquareRoot
    | otherwise = Right (sqrt x)

safeDivideEither :: Float -> Float -> Either MathError Float
safeDivideEither _ 0 = Left DivisionByZero
safeDivideEither x y = Right (x / y)

-- 모나드 체이닝 예제
-- 복잡한 수학 연산 체이닝
complexMath :: Float -> Float -> Either MathError Float
complexMath x y = do
    sqrtX <- safeSqrt x
    sqrtY <- safeSqrt y
    result <- safeDivideEither sqrtX sqrtY
    return (result * 2)

-- do 표기법을 사용한 Maybe 처리
processUserInput :: String -> Maybe String
processUserInput input = do
    let trimmed = trim input
    guard (not (null trimmed))
    guard (length trimmed >= 3)
    return (map toUpper trimmed)

-- Functor 예제
-- Maybe에 함수 적용
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe f Nothing = Nothing
applyToMaybe f (Just x) = Just (f x)

-- Applicative 예제
-- 여러 Maybe 값에 함수 적용
combineMaybes :: Maybe Int -> Maybe Int -> Maybe Int
combineMaybes mx my = (+) <$> mx <*> my

-- 커스텀 모나드 예제
-- 간단한 상태 모나드
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    State f <*> State g = State $ \s -> let (h, s') = f s; (a, s'') = g s' in (h a, s'')

instance Monad (State s) where
    return = pure
    State f >>= g = State $ \s -> let (a, s') = f s in runState (g a) s'

-- 상태 모나드 사용 예제
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- 카운터 예제
increment :: State Int Int
increment = do
    count <- get
    put (count + 1)
    return count

-- 유틸리티 함수들
trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 타입 클래스와 모나드 기초 예제 ==="
    putStrLn ""
    
    -- 타입 클래스 예제
    putStrLn "타입 클래스 예제:"
    let person1 = Person "Alice" 25
        person2 = Person "Bob" 30
    putStrLn $ "Person1: " ++ show person1
    putStrLn $ "Person2: " ++ show person2
    putStrLn $ "person1 < person2: " ++ show (person1 < person2)
    putStrLn ""
    
    -- Maybe 타입 예제
    putStrLn "Maybe 타입 예제:"
    putStrLn $ "safeDivide 10 2: " ++ show (safeDivide 10 2)
    putStrLn $ "safeDivide 10 0: " ++ show (safeDivide 10 0)
    putStrLn $ "findElement 3 [1,2,3,4,5]: " ++ show (findElement 3 [1,2,3,4,5])
    putStrLn $ "findElement 6 [1,2,3,4,5]: " ++ show (findElement 6 [1,2,3,4,5])
    putStrLn ""
    
    -- Either 타입 예제
    putStrLn "Either 타입 예제:"
    putStrLn $ "safeSqrt 16: " ++ show (safeSqrt 16)
    putStrLn $ "safeSqrt (-1): " ++ show (safeSqrt (-1))
    putStrLn $ "safeDivideEither 10 2: " ++ show (safeDivideEither 10 2)
    putStrLn $ "safeDivideEither 10 0: " ++ show (safeDivideEither 10 0)
    putStrLn ""
    
    -- 모나드 체이닝 예제
    putStrLn "모나드 체이닝 예제:"
    putStrLn $ "complexMath 16 4: " ++ show (complexMath 16 4)
    putStrLn $ "complexMath 16 (-4): " ++ show (complexMath 16 (-4))
    putStrLn ""
    
    -- Functor와 Applicative 예제
    putStrLn "Functor와 Applicative 예제:"
    putStrLn $ "applyToMaybe (*2) (Just 5): " ++ show (applyToMaybe (*2) (Just 5))
    putStrLn $ "applyToMaybe (*2) Nothing: " ++ show (applyToMaybe (*2) Nothing)
    putStrLn $ "combineMaybes (Just 5) (Just 3): " ++ show (combineMaybes (Just 5) (Just 3))
    putStrLn $ "combineMaybes (Just 5) Nothing: " ++ show (combineMaybes (Just 5) Nothing)
    putStrLn ""
    
    -- 상태 모나드 예제
    putStrLn "상태 모나드 예제:"
    let (result, finalState) = runState increment 0
    putStrLn $ "increment from 0: result=" ++ show result ++ ", finalState=" ++ show finalState
    let (result2, finalState2) = runState (increment >> increment) 0
    putStrLn $ "increment twice from 0: result=" ++ show result2 ++ ", finalState=" ++ show finalState2
