# 1단계: 기본 문법과 타입 시스템

## 학습 목표
- Haskell의 기본 문법 이해
- 타입 시스템과 타입 추론 학습
- 변수와 함수 정의 방법 익히기
- 기본 타입들 (Int, Float, Bool, Char, String) 사용법

## 학습 내용

### 1. 기본 타입
- `Int`: 정수
- `Float`: 부동소수점
- `Bool`: 불린 (True/False)
- `Char`: 문자
- `String`: 문자열 (Char의 리스트)

### 2. 변수와 함수
- `let` 키워드를 사용한 변수 정의
- 함수 정의 문법
- 타입 시그니처 작성법

### 3. 타입 추론
- Haskell의 강력한 타입 추론 시스템
- 명시적 타입 선언의 장점

## 프로젝트: 간단한 계산기

### 구현할 기능
1. 기본 사칙연산 함수들
2. 타입 변환 함수들
3. 간단한 수학 함수들

### 예제 코드
```haskell
-- 기본 타입 예제
-- Int 타입
age :: Int
age = 25

-- Float 타입
pi :: Float
pi = 3.14159

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
```

## 연습 문제
1. 두 정수의 최대값을 반환하는 함수를 작성하세요
2. 세 개의 정수를 받아서 평균을 계산하는 함수를 작성하세요
3. 문자열의 길이를 계산하는 함수를 작성하세요 (String은 [Char]이므로 length 함수 사용)
4. 정수를 받아서 절댓값을 반환하는 함수를 작성하세요

## 테스트 방법
```bash
# GHCi에서 테스트
ghci
:load Main.hs
add 5 3
multiply 2.5 4.0
```

## 다음 단계
2단계에서는 리스트와 패턴 매칭에 대해 학습합니다.
