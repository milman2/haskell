# 2단계: 리스트와 패턴 매칭

## 학습 목표
- 리스트의 생성과 조작 방법 학습
- 패턴 매칭을 통한 데이터 분해
- 가드(Guard)를 사용한 조건부 로직
- 리스트 컴프리헨션 이해

## 학습 내용

### 1. 리스트 기본
- 리스트 생성: `[1,2,3]`, `1:2:3:[]`
- 빈 리스트: `[]`
- 리스트 연결: `++` 연산자
- 리스트 접근: `head`, `tail`, `last`, `init`

### 2. 패턴 매칭
- 값 패턴 매칭
- 리스트 패턴 매칭
- 튜플 패턴 매칭
- 와일드카드 패턴 (`_`)

### 3. 가드 (Guards)
- `|` (파이프)를 사용한 조건부 로직
- `otherwise` 키워드

### 4. 리스트 컴프리헨션
- `[x | x <- [1..10], even x]` 형태의 문법

## 프로젝트: 리스트 처리 유틸리티와 간단한 게임

### 구현할 기능
1. 리스트 조작 함수들
2. 숫자 맞추기 게임
3. 간단한 통계 함수들

### 예제 코드
```haskell
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
```

## 연습 문제
1. 리스트에서 최댓값을 찾는 함수를 작성하세요
2. 리스트를 뒤집는 함수를 작성하세요
3. 두 리스트를 합치는 함수를 작성하세요
4. 리스트에서 특정 요소의 개수를 세는 함수를 작성하세요
5. 리스트에서 중복을 제거하는 함수를 작성하세요

## 테스트 방법
```bash
ghci
:load Main.hs
sumList [1,2,3,4,5]
grade 85
evens [1,2,3,4,5,6,7,8,9,10]
```

## 다음 단계
3단계에서는 재귀와 고차 함수에 대해 학습합니다.
