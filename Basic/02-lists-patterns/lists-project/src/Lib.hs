module Lib
    ( someFunc
    , listLength
    , sumList
    , firstElement
    , grade
    , evens
    , pythagoreanTriples
    , guessNumber
    , findMax
    , reverseList
    , mergeLists
    , countElement
    , removeDuplicates
    ) where

-- 리스트 기본 함수들
-- 리스트의 길이 계산
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- 리스트의 합계 계산
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- 패턴 매칭 예제
-- 리스트의 첫 번째 요소 반환
firstElement :: [a] -> Maybe a
firstElement [] = Nothing
firstElement (x:_) = Just x

-- 가드를 사용한 조건부 함수
-- 성적에 따른 등급 계산
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise = "F"

-- 리스트 컴프리헨션 예제
-- 짝수만 필터링
evens :: [Int] -> [Int]
evens xs = [x | x <- xs, even x]

-- 피타고라스 삼각형 찾기
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2]

-- 간단한 게임: 숫자 맞추기
-- 사용자가 생각한 숫자를 맞추는 함수
guessNumber :: Int -> Int -> String
guessNumber target guess
    | guess == target = "정답입니다!"
    | guess < target = "더 큰 수입니다."
    | otherwise = "더 작은 수입니다."

-- 연습 문제 해답들
-- 1. 리스트에서 최댓값을 찾는 함수
findMax :: [Int] -> Maybe Int
findMax [] = Nothing
findMax [x] = Just x
findMax (x:xs) = case findMax xs of
    Nothing -> Just x
    Just maxRest -> Just (if x > maxRest then x else maxRest)

-- 2. 리스트를 뒤집는 함수
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- 3. 두 리스트를 합치는 함수
mergeLists :: [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) ys = x : mergeLists xs ys

-- 4. 리스트에서 특정 요소의 개수를 세는 함수
countElement :: Eq a => a -> [a] -> Int
countElement _ [] = 0
countElement target (x:xs)
    | x == target = 1 + countElement target xs
    | otherwise = countElement target xs

-- 5. 리스트에서 중복을 제거하는 함수
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 리스트와 패턴 매칭 예제 ==="
    putStrLn ""
    
    -- 리스트 기본 함수들
    putStrLn "리스트 기본 함수들:"
    putStrLn $ "[1,2,3,4,5]의 길이: " ++ show (listLength [1,2,3,4,5])
    putStrLn $ "[1,2,3,4,5]의 합계: " ++ show (sumList [1,2,3,4,5])
    putStrLn $ "[1,2,3]의 첫 번째 요소: " ++ show (firstElement [1,2,3])
    putStrLn $ "빈 리스트의 첫 번째 요소: " ++ show (firstElement ([] :: [Int]))
    putStrLn ""
    
    -- 가드 예제
    putStrLn "가드 예제 (성적 등급):"
    putStrLn $ "95점: " ++ grade 95
    putStrLn $ "85점: " ++ grade 85
    putStrLn $ "75점: " ++ grade 75
    putStrLn $ "65점: " ++ grade 65
    putStrLn $ "55점: " ++ grade 55
    putStrLn ""
    
    -- 리스트 컴프리헨션 예제
    putStrLn "리스트 컴프리헨션 예제:"
    putStrLn $ "[1..10]에서 짝수만: " ++ show (evens [1..10])
    putStrLn $ "피타고라스 삼각형 (처음 5개): " ++ show (take 5 pythagoreanTriples)
    putStrLn ""
    
    -- 게임 예제
    putStrLn "숫자 맞추기 게임:"
    putStrLn $ "목표: 7, 추측: 5 -> " ++ guessNumber 7 5
    putStrLn $ "목표: 7, 추측: 9 -> " ++ guessNumber 7 9
    putStrLn $ "목표: 7, 추측: 7 -> " ++ guessNumber 7 7
    putStrLn ""
    
    -- 연습 문제 해답
    putStrLn "연습 문제 해답:"
    putStrLn $ "[3,1,4,1,5,9]의 최댓값: " ++ show (findMax [3,1,4,1,5,9])
    putStrLn $ "[1,2,3,4,5]를 뒤집기: " ++ show (reverseList [1,2,3,4,5])
    putStrLn $ "[1,2,3]과 [4,5,6] 합치기: " ++ show (mergeLists [1,2,3] [4,5,6])
    putStrLn $ "[1,2,2,3,2,4]에서 2의 개수: " ++ show (countElement 2 [1,2,2,3,2,4])
    putStrLn $ "[1,2,2,3,2,4]에서 중복 제거: " ++ show (removeDuplicates [1,2,2,3,2,4])
