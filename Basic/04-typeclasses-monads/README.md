# 4단계: 타입 클래스와 모나드 기초

## 학습 목표
- 타입 클래스의 개념과 사용법 이해
- `Maybe`와 `Either` 타입을 통한 안전한 프로그래밍
- 모나드의 기본 개념 학습
- `Functor`, `Applicative`, `Monad` 타입 클래스 이해

## 학습 내용

### 1. 타입 클래스
- 타입 클래스 정의와 인스턴스 구현
- 기본 타입 클래스들: `Eq`, `Ord`, `Show`, `Read`
- 커스텀 타입 클래스 만들기

### 2. Maybe 타입
- `Nothing`과 `Just`를 사용한 옵셔널 값 처리
- Maybe 모나드의 활용

### 3. Either 타입
- `Left`와 `Right`를 사용한 에러 처리
- Either 모나드의 활용

### 4. 모나드 기초
- 모나드의 세 가지 법칙
- `do` 표기법
- 모나드 체이닝

### 5. Functor와 Applicative
- `fmap`과 `<$>` 연산자
- `pure`와 `<*>` 연산자

## 프로젝트: 에러 처리와 옵셔널 값 처리

### 구현할 기능
1. 안전한 나눗셈과 수학 연산
2. 파일 시스템 시뮬레이션
3. 사용자 입력 검증 시스템

### 예제 코드
```haskell
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
```

## 연습 문제
1. Maybe를 사용하여 리스트의 첫 번째 요소를 안전하게 가져오는 함수를 작성하세요
2. Either를 사용하여 문자열을 정수로 변환하는 함수를 작성하세요
3. Maybe를 사용하여 딕셔너리(연관 리스트)에서 값을 찾는 함수를 작성하세요
4. do 표기법을 사용하여 여러 Maybe 값을 조합하는 함수를 작성하세요
5. Functor를 사용하여 리스트의 모든 Maybe 값을 처리하는 함수를 작성하세요

## 고급 연습 문제
1. 간단한 파서 모나드를 구현하세요
2. Writer 모나드를 구현하여 로깅 기능을 추가하세요
3. Reader 모나드를 구현하여 환경 설정을 전달하세요

## 테스트 방법
```bash
ghci
:load Main.hs
safeDivide 10 2
safeDivide 10 0
safeSqrt 16
safeSqrt (-1)
complexMath 16 4
combineMaybes (Just 5) (Just 3)
```

## 다음 단계
5단계에서는 입출력과 IO 모나드에 대해 학습합니다.
