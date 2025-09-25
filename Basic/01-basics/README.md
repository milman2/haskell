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
cd /home/milman2/haskell/Basic/01-basics/basics-project/
# GHCi에서 테스트
ghci # stack ghci
:load app/Main.hs
add 5 3
multiply 2.5 4.0
```

## 다음 단계
2단계에서는 리스트와 패턴 매칭에 대해 학습합니다.

# Curring
- Int -> Int -> Int는 Int -> (Int -> Int)와 같음
# 타입(Types)
- :: (type signiture)
# 다형 타입(Polymorphic types)
- type variable : 소문자
# 사용자 정의 타입(User-defined types)
- data
- 재귀 타입 (ex. Tre. List)
# 고차원 함수(Higher-order functions)
- 함수를 결과로 리턴
- 한수를 인자로 주기
```
add :: Integer -> Integer -> Integer
add x y = x + y

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs
```
# 지연 계산법(Lazy evaluation)
- 무한 자료 구조(infinite data structure)
- 필요할 때만 계산
```
ones = 1 : ones
numFrom n = n : numFrom (n+1)
squares = map (\x -> x^2) (numFrom 0)
fib = 1 : 1 : [a + b | (a,b) <- zip fib (tail fib)]
```

# 람다 계산법(Lambda calculus) - Alonzo Church
- 튜링 완정성 (Turing-completeness) : 컴퓨터가 하는 모든 일을 할 수 있음(논리 연산, 산술 연산, 메모리)
```shell
# 구문(Syntax)
## 변수 - Variable
x 
## 추상화 - Abstraction, (이름이 없는) 함수 정의
λx. M 
## 적용 - Application, 함수 호출
M N

# 의미(Semantics)
(λx. M) N => M [x:=N]

(λx. x+1) 123 => (x+1) [x:=123] = (123 + 1) = 124
```

## 논리곱
```shell
tru = λt. λf. t # True
fls = λt. λf. f # False
and = λb. λc. b c fls
and tru tru = ((λb. λc. b c fls) tru) tru
    => (λc. b c fls) [b:=tru] tru = (λc. tru c fls) tru
    => (λn. tru c fls) [c:=tru] = (tru tru fls) = (λt. λf. t) tru fls
    => ((λf. t)[t:=tru]) fls = (λf. tru) fls
    => ((tru)[f:=fls]) = tru
```
## 람다(Lambda) - Anonymous function
```shell
ghci> (\x -> length x > 1) "abc"
ghci> filter (\x -> length x > 1) ["abc", "d", "ef"]
```

## 숫자와 산술 연산 - Church numerals and arithmetic operations
```shell
c0 = λs. λz. z
c1 = λs. λz. s z
c2 = λs. λz. s (s z)
c3 = λs. λz. s (s (s z))

plus = λm. λn. λs. λz. m s (n s z)
c3 = plus c1 c2
```
# Partial application
```shell
ghci> (*3) 4
ghci> (3*) 4
```

# 타입 시스템 (Type system)
- Monad type
- Gradual types
- Session typed calculus
- The Polymorphic RPC calculus