# Haskell 모나드(Monad)

## 모나드(Monad)란?

- 모나드는 **순차적인 계산을 체계적으로 처리**할 수 있게 해주는 타입 클래스입니다. 부작용(side effects)이 있는 계산을 안전하게 다루는 데 사용됩니다.
- **계산을 연결하는 방식**을 정의하는 추상화

## 모나드 법칙 (Monad Laws)

### 1. **Left Identity**
```haskell
return x >>= f ≡ f x
```

### 2. **Right Identity**
```haskell
m >>= return ≡ m
```

### 3. **Associativity**
```haskell
(m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
```

```hs
let f x = Just (x + 1)
let g x = Just (x * 10)
(Just 1 >>= f) >>= g -- Just 20
Just 1 >>= (\x -> f x >>= g) -- Just 20
```

## 모나드의 기본 개념

### 1. **모나드 타입 클래스 정의**

```haskell
class (Applicative m) => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b  -- bind 연산자
    return :: a -> m a                 -- 값을 모나드로 감싸기. (이제는 `pure`로 대체됨)
    return = pure
    (>>) :: m a -> m b -> m b          -- then 순차 실행
    fail :: String -> m a              -- 실패 처리
```

### 예시
```hs
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

calc :: Maybe Int
calc = Just 10 >>= \x ->
       Just 2  >>= \y ->
       safeDivide x y
```


### 2. **핵심 연산자들**

#### **`>>=` (bind 연산자)**
```haskell
-- 타입: m a -> (a -> m b) -> m b
-- 의미: 모나드 값을 받아서 함수를 적용하고 결과를 모나드로 반환

-- 예시
maybeValue >>= \x -> Just (x + 1)
```

#### **`return`**
```haskell
-- 타입: a -> m a
-- 의미: 일반 값을 모나드로 감싸기

-- 예시
return 5 :: Maybe Int  -- Just 5
return "hello" :: IO String  -- IO String
```

## 주요 모나드 타입들

### 1. **Maybe 모나드 (Optional Values)**

```haskell
-- Maybe 타입 정의
data Maybe a = Nothing | Just a

-- Maybe 모나드 인스턴스
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x

-- 사용 예시
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- 체이닝 예시
calculate :: Int -> Int -> Maybe Int
calculate x y = do
    result1 <- safeDiv x y
    result2 <- safeDiv result1 2
    return result2

-- 테스트
test1 = calculate 10 2   -- Just 2
test2 = calculate 10 0   -- Nothing
```

### 2. **Either 모나드 (Error Handling)**

```haskell
-- Either 타입 정의
data Either a b = Left a | Right b

-- Either 모나드 인스턴스
instance Monad (Either e) where
    return x = Right x
    Left e >>= f = Left e
    Right x >>= f = f x

-- 사용 예시
parseInt :: String -> Either String Int
parseInt s = case reads s of
    [(n, "")] -> Right n
    _         -> Left ("Cannot parse: " ++ s)

-- 체이닝 예시
processNumber :: String -> Either String Int
processNumber s = do
    n <- parseInt s
    if n < 0
        then Left "Negative number not allowed"
        else Right (n * 2)

-- 테스트
test1 = processNumber "5"    -- Right 10
test2 = processNumber "abc"  -- Left "Cannot parse: abc"
test3 = processNumber "-3"   -- Left "Negative number not allowed"
```

### 3. **IO 모나드 (Input/Output)**

```haskell
-- IO 모나드는 Haskell 런타임에서 제공
-- 외부 세계와의 상호작용을 안전하게 처리

-- 사용 예시
main :: IO ()
main = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

-- 파일 처리 예시
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()

-- 파일 복사 예시
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
    content <- readFile src
    writeFile dst content
```

### 4. **리스트 모나드 (Non-deterministic Computation)**

```haskell
-- 리스트 모나드 인스턴스
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)

-- 사용 예시
-- 모든 가능한 조합 생성
combinations :: [Int] -> [Int] -> [(Int, Int)]
combinations xs ys = do
    x <- xs
    y <- ys
    return (x, y)

-- 테스트
test = combinations [1,2] [3,4]  -- [(1,3),(1,4),(2,3),(2,4)]

-- 피타고라스 삼각형 찾기
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = do
    a <- [1..n]
    b <- [1..n]
    c <- [1..n]
    if a^2 + b^2 == c^2
        then return (a, b, c)
        else []
```

### 5. **State 모나드 (Stateful Computation)**

```haskell
-- State 타입 정의
newtype State s a = State { runState :: s -> (a, s) }

-- State 모나드 인스턴스
instance Monad (State s) where
    return x = State $ \s -> (x, s)
    st >>= f = State $ \s ->
        let (a, s') = runState st s
        in runState (f a) s'

-- 사용 예시
-- 카운터 예시
type Counter = State Int

increment :: Counter Int
increment = State $ \s -> (s, s + 1)

getCount :: Counter Int
getCount = State $ \s -> (s, s)

-- 체이닝 예시
incrementTwice :: Counter Int
incrementTwice = do
    _ <- increment
    _ <- increment
    getCount

-- 테스트
test = runState incrementTwice 0  -- (2, 2)
```

## do 표기법 (do Notation)
- **명령형 스타일**로 표현 가능
### 1. **기본 사용법**

```haskell
-- >>= 연산자 사용
result = maybeValue >>= \x -> 
         anotherValue >>= \y -> 
         return (x + y)

-- do 표기법 사용 (더 읽기 쉬움)
result = do
    x <- maybeValue
    y <- anotherValue
    return (x + y)
```

### 2. **실제 예시**

```haskell
-- Maybe 모나드 예시
safeCalculation :: Int -> Int -> Maybe Int
safeCalculation x y = do
    result1 <- safeDiv x y
    result2 <- safeDiv result1 2
    result3 <- safeDiv result2 3
    return result3

-- IO 모나드 예시
interactiveProgram :: IO ()
interactiveProgram = do
    putStrLn "Enter first number:"
    input1 <- getLine
    putStrLn "Enter second number:"
    input2 <- getLine
    let num1 = read input1 :: Int
    let num2 = read input2 :: Int
    putStrLn $ "Sum: " ++ show (num1 + num2)
```



## 모나드 유틸리티 함수들

### 1. **`>>` (then 연산자)**
```haskell
-- 타입: m a -> m b -> m b
-- 의미: 첫 번째 모나드를 실행하고 결과를 무시

putStrLn "Hello" >> putStrLn "World"
```

### 2. **`when`**
```haskell
-- 타입: Bool -> m () -> m ()
-- 의미: 조건이 참일 때만 모나드 실행

when (x > 0) $ putStrLn "Positive number"
```

### 3. **`unless`**
```haskell
-- 타입: Bool -> m () -> m ()
-- 의미: 조건이 거짓일 때만 모나드 실행

unless (x < 0) $ putStrLn "Non-negative number"
```

### 4. **`sequence`**
```haskell
-- 타입: [m a] -> m [a]
-- 의미: 모나드 리스트를 순차적으로 실행

sequence [putStrLn "1", putStrLn "2", putStrLn "3"]
```

### 5. **`mapM`**
```haskell
-- 타입: (a -> m b) -> [a] -> m [b]
-- 의미: 리스트의 각 요소에 모나드 함수 적용

mapM putStrLn ["Hello", "World", "Haskell"]
```

## 커스텀 모나드 만들기

### 1. **간단한 로깅 모나드**

```haskell
-- 로깅 모나드 정의
newtype Logger a = Logger (a, [String])

-- Logger 모나드 인스턴스
instance Monad Logger where
    return x = Logger (x, [])
    Logger (x, logs) >>= f = 
        let Logger (y, newLogs) = f x
        in Logger (y, logs ++ newLogs)

-- 로깅 함수
logMessage :: String -> Logger ()
logMessage msg = Logger ((), [msg])

-- 사용 예시
calculation :: Int -> Logger Int
calculation x = do
    logMessage $ "Starting calculation with " ++ show x
    let result = x * 2
    logMessage $ "Result: " ++ show result
    return result

-- 테스트
test = calculation 5  -- Logger (10, ["Starting calculation with 5", "Result: 10"])
```

### 2. **Reader 모나드 (Environment Passing)**

```haskell
-- Reader 모나드 정의
newtype Reader r a = Reader { runReader :: r -> a }

-- Reader 모나드 인스턴스
instance Monad (Reader r) where
    return x = Reader $ \_ -> x
    Reader f >>= g = Reader $ \r ->
        let a = f r
        in runReader (g a) r

-- 환경 조회 함수
ask :: Reader r r
ask = Reader id

-- 환경 변환 함수
local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader $ g . f

-- 사용 예시
type Config = (String, Int)

getConfig :: Reader Config String
getConfig = do
    (name, _) <- ask
    return name

getPort :: Reader Config Int
getPort = do
    (_, port) <- ask
    return port

-- 테스트
config = ("localhost", 8080)
test1 = runReader getConfig config  -- "localhost"
test2 = runReader getPort config    -- 8080
```

## 모나드 변환자 (Monad Transformers)

### 1. **MaybeT (Maybe Transformer)**

```haskell
-- MaybeT 정의
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- MaybeT 모나드 인스턴스
instance Monad m => Monad (MaybeT m) where
    return x = MaybeT $ return (Just x)
    MaybeT m >>= f = MaybeT $ do
        maybeVal <- m
        case maybeVal of
            Nothing -> return Nothing
            Just x  -> runMaybeT (f x)

-- 사용 예시
type IOWithMaybe = MaybeT IO

safeReadFile :: FilePath -> IOWithMaybe String
safeReadFile path = MaybeT $ do
    exists <- doesFileExist path
    if exists
        then fmap Just (readFile path)
        else return Nothing
```

## 모나드의 장점

### 1. **부작용 관리**
```haskell
-- IO 모나드로 외부 세계와의 상호작용을 안전하게 처리
main :: IO ()
main = do
    content <- readFile "input.txt"
    writeFile "output.txt" (process content)
```

### 2. **에러 처리**
```haskell
-- Maybe/Either 모나드로 에러를 체계적으로 처리
safeOperation :: Int -> Maybe Int
safeOperation x = do
    y <- safeDiv x 2
    z <- safeDiv y 3
    return z
```

### 3. **코드 재사용**
```haskell
-- 모나드 함수들을 조합하여 복잡한 계산 구성
complexCalculation :: Maybe Int
complexCalculation = do
    x <- getValue
    y <- processValue x
    z <- validateValue y
    return z
```

### 4. **순차적 계산**
```haskell
-- do 표기법으로 순차적 계산을 명확하게 표현
processData :: IO ()
processData = do
    input <- getLine
    let processed = process input
    putStrLn processed
    saveToFile processed
```

## 모나드 vs 다른 패턴

### 1. **모나드 vs Functor**
```haskell
-- Functor: fmap :: (a -> b) -> f a -> f b
-- Monad:   (>>=) :: m a -> (a -> m b) -> m b

-- Functor는 단순한 변환
fmap (+1) (Just 5)  -- Just 6

-- Monad는 복잡한 계산 체이닝
Just 5 >>= \x -> Just (x + 1) >>= \y -> Just (y * 2)  -- Just 12
```

### 2. **모나드 vs Applicative**
```haskell
-- Applicative: <*> :: f (a -> b) -> f a -> f b
-- Monad:       (>>=) :: m a -> (a -> m b) -> m b

-- Applicative는 독립적인 계산
(+) <$> Just 5 <*> Just 3  -- Just 8

-- Monad는 의존적인 계산
Just 5 >>= \x -> Just (x + 3)  -- Just 8
```

## 실제 사용 예시

### 1. **웹 서버 예시**
```haskell
-- 간단한 웹 서버 처리
handleRequest :: Request -> IO Response
handleRequest req = do
    user <- authenticateUser req
    content <- fetchContent user
    response <- generateResponse content
    logRequest req response
    return response
```

### 2. **데이터베이스 처리**
```haskell
-- 데이터베이스 트랜잭션
processOrder :: OrderId -> IO (Either String Order)
processOrder orderId = do
    order <- fetchOrder orderId
    case order of
        Nothing -> return $ Left "Order not found"
        Just o -> do
            inventory <- checkInventory o
            if inventory
                then do
                    updateInventory o
                    confirmOrder o
                    return $ Right o
                else return $ Left "Insufficient inventory"
```

## 요약

모나드는:
- **순차적 계산**: `>>=` 연산자로 계산을 체계적으로 연결
- **부작용 관리**: IO, State 등으로 부작용을 안전하게 처리
- **에러 처리**: Maybe, Either로 에러를 체계적으로 처리
- **코드 조합**: 작은 모나드 함수들을 조합하여 복잡한 계산 구성
- **do 표기법**: `>>=` 연산자를 더 읽기 쉽게 만드는 문법 설탕
- **법칙 준수**: Left Identity, Right Identity, Associativity 법칙을 만족
- **확장성**: 모나드 변환자로 여러 모나드를 조합 가능

Haskell에서 부작용이 있는 계산을 안전하고 체계적으로 처리하는 핵심 도구입니다!

## 참고 자료

- [Haskell Monad 공식 문서](https://www.haskell.org/tutorial/monads.html)
- [Learn You a Haskell - Monads](http://learnyouahaskell.com/a-fistful-of-monads)
- [Real World Haskell - Monads](http://book.realworldhaskell.org/read/monads.html)
- [All About Monads](https://wiki.haskell.org/All_About_Monads)
