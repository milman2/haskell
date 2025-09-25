# 3단계: 재귀와 고차 함수

## 학습 목표
- 재귀 함수의 작성과 이해
- 고차 함수 (Higher-Order Functions) 개념 학습
- `map`, `filter`, `fold` 함수들의 활용
- 함수 합성과 부분 적용

## 학습 내용

### 1. 재귀 함수
- 기본 케이스와 재귀 케이스
- 꼬리 재귀 (Tail Recursion)
- 재귀를 사용한 알고리즘 구현

### 2. 고차 함수
- 함수를 인자로 받는 함수
- 함수를 반환하는 함수
- 익명 함수 (Lambda Expressions)

### 3. 리스트 고차 함수
- `map`: 모든 요소에 함수 적용
- `filter`: 조건을 만족하는 요소만 필터링
- `foldl`, `foldr`: 리스트를 하나의 값으로 축약
- `zipWith`: 두 리스트를 결합

### 4. 함수 합성
- `(.)` 연산자를 사용한 함수 합성
- `$` 연산자를 사용한 함수 적용

## 프로젝트: 정렬 알고리즘과 데이터 변환

### 구현할 기능
1. 다양한 정렬 알고리즘 (버블, 퀵, 머지)
2. 데이터 변환 파이프라인
3. 함수형 프로그래밍 스타일의 유틸리티

### 예제 코드
```haskell
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
processNumbers = map (* 2) . filter even . map (+ 1)

-- Lambda 표현식 예제
-- 익명 함수 사용
squareAll :: [Int] -> [Int]
squareAll xs = map (\x -> x * x) xs
```

## 연습 문제
1. 리스트의 모든 요소를 제곱하는 함수를 작성하세요
2. 문자열 리스트에서 길이가 5 이상인 것만 필터링하는 함수를 작성하세요
3. 리스트의 모든 요소를 곱하는 함수를 작성하세요
4. 두 리스트의 내적을 계산하는 함수를 작성하세요
5. 리스트를 받아서 각 요소를 두 번 반복하는 함수를 작성하세요

## 고급 연습 문제
1. 머지 정렬을 구현하세요
2. 이진 탐색을 구현하세요
3. 함수 합성을 사용하여 데이터 변환 파이프라인을 만드세요

## 테스트 방법
```bash
ghci
:load Main.hs
factorial 5
fibonacci 10
applyTwice (*2) 3
quickSort [3,1,4,1,5,9,2,6]
processNumbers [1,2,3,4,5,6,7,8,9,10]
```

## 다음 단계
4단계에서는 타입 클래스와 모나드 기초에 대해 학습합니다.
