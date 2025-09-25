# Haskell 타입 선언 연산자 (::)

## 타입 선언 연산자란?

Haskell에서 `::`는 **타입 선언 연산자(Type Declaration Operator)**입니다. 이 연산자는 함수나 값의 타입을 명시적으로 선언할 때 사용됩니다.

## 기본 문법

```haskell
식별자 :: 타입
```

- **식별자**: 함수명, 변수명, 값의 이름
- **`::`**: "의 타입은" 또는 "has type"을 의미
- **타입**: 해당 식별자의 타입

## `::` 뒤에 올 수 있는 타입들

### 1. **기본 타입 (Basic Types)**

```haskell
-- 숫자 타입
x :: Int          -- 정수
y :: Integer      -- 큰 정수
z :: Float        -- 부동소수점
w :: Double       -- 더 정밀한 부동소수점

-- 문자 타입
c :: Char         -- 단일 문자
s :: String       -- 문자열 (Char의 리스트)
t :: Text         -- 효율적인 문자열

-- 불린 타입
b :: Bool         -- True 또는 False
```

### 2. **리스트 타입 (List Types)**

```haskell
-- 기본 리스트
numbers :: [Int]           -- 정수 리스트
words :: [String]          -- 문자열 리스트
chars :: [Char]            -- 문자 리스트

-- 중첩 리스트
matrix :: [[Int]]          -- 2차원 정수 배열
nested :: [[[String]]]     -- 3차원 문자열 배열
```

### 3. **튜플 타입 (Tuple Types)**

```haskell
-- 2-튜플 (페어)
pair :: (Int, String)      -- 정수와 문자열의 쌍
coord :: (Double, Double)  -- 좌표

-- 3-튜플
triple :: (Int, String, Bool)  -- 세 개의 값

-- 더 많은 튜플
quad :: (Int, String, Bool, Double)  -- 네 개의 값
```

### 4. **함수 타입 (Function Types)**

```haskell
-- 단순 함수
add :: Int -> Int -> Int   -- 두 정수를 받아 정수 반환
greet :: String -> String  -- 문자열을 받아 문자열 반환

-- 고차 함수
apply :: (Int -> Int) -> Int -> Int  -- 함수를 받는 함수
map :: (a -> b) -> [a] -> [b]        -- 리스트의 각 요소에 함수 적용
```

### 5. **Maybe 타입 (Optional Types)**

```haskell
-- Maybe 타입
maybeInt :: Maybe Int      -- 정수이거나 Nothing
maybeString :: Maybe String -- 문자열이거나 Nothing

-- 사용 예시
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
```

### 6. **Either 타입 (Error Handling)**

```haskell
-- Either 타입
result :: Either String Int  -- 문자열(에러) 또는 정수(성공)
parseResult :: Either ParseError Text  -- 파싱 에러 또는 텍스트
```

### 7. **커스텀 타입 (Custom Types)**

```haskell
-- 데이터 타입 정의
data Color = Red | Green | Blue
data Person = Person String Int

-- 사용
myColor :: Color
myColor = Red

myPerson :: Person
myPerson = Person "Alice" 25
```

### 8. **타입 변수 (Type Variables)**

```haskell
-- 제네릭 타입
identity :: a -> a         -- 어떤 타입이든 받아서 같은 타입 반환
first :: (a, b) -> a       -- 튜플의 첫 번째 요소
length :: [a] -> Int       -- 어떤 타입의 리스트든 길이 반환
```

### 9. **타입 클래스 (Type Classes)**

```haskell
-- 타입 클래스 제약
showable :: Show a => a -> String  -- Show 인스턴스가 있는 타입
comparable :: Ord a => a -> a -> Bool  -- 비교 가능한 타입
```

### 10. **모나드 타입 (Monad Types)**

```haskell
-- Parser 모나드
identifier :: Parser Text
number :: Parser Int

-- IO 모나드
main :: IO ()
getLine :: IO String
putStrLn :: String -> IO ()

-- Maybe 모나드
safeHead :: [a] -> Maybe a
```

### 11. **복합 타입 (Complex Types)**

```haskell
-- 함수와 모나드 조합
parseAndProcess :: Parser Text -> String -> Either ParseError Text
-- Parser Text를 받아서 String을 파싱하고 Either로 결과 반환

-- 중첩된 타입
nestedMaybe :: Maybe (Either String Int)
-- Maybe 안에 Either가 들어있음

-- 함수의 함수
higherOrder :: (Int -> Int) -> (Int -> Int) -> Int -> Int
-- 함수를 받아서 함수를 반환하는 함수
```

### 12. **타입 별칭 (Type Synonyms)**

```haskell
-- 타입 별칭 정의
type Name = String
type Age = Int
type Person = (Name, Age)

-- 사용
myName :: Name
myAge :: Age
myPerson :: Person
```

## 타입 선언의 패턴

### **기본 패턴**
```haskell
함수명 :: 입력타입1 -> 입력타입2 -> ... -> 출력타입
```

### **예시들**
```haskell
-- 0개 입력 (상수)
pi :: Double

-- 1개 입력
square :: Int -> Int

-- 2개 입력
add :: Int -> Int -> Int

-- 3개 입력
combine :: String -> Int -> Bool -> String

-- 리스트 입력
sum :: [Int] -> Int

-- 함수 입력
apply :: (Int -> Int) -> Int -> Int
```

## 실제 사용 예시

### 1. **간단한 함수 선언**

```haskell
-- 기본 함수
double :: Int -> Int
double x = x * 2

-- 문자열 처리
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- 리스트 처리
head :: [a] -> a
head (x:_) = x
```

### 2. **복잡한 함수 선언**

```haskell
-- 고차 함수
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- 모나드 함수
parseInt :: String -> Maybe Int
parseInt s = case reads s of
    [(n, "")] -> Just n
    _         -> Nothing

-- 타입 클래스 제약이 있는 함수
showList :: Show a => [a] -> String
showList xs = "[" ++ intercalate ", " (map show xs) ++ "]"
```

### 3. **데이터 타입과 함께 사용**

```haskell
-- 데이터 타입 정의
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- 타입 선언
treeSize :: Tree a -> Int
treeSize (Leaf _) = 1
treeSize (Node left right) = treeSize left + treeSize right

-- 제네릭 함수
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf x) = Leaf (f x)
treeMap f (Node left right) = Node (treeMap f left) (treeMap f right)
```

## 타입 선언의 장점

### 1. **명확성**
```haskell
-- 타입이 명확하게 드러남
add :: Int -> Int -> Int
add x y = x + y

-- 타입 없이는 모호할 수 있음
add x y = x + y  -- 어떤 타입인지 불분명
```

### 2. **문서화**
```haskell
-- 함수의 목적이 타입에서 드러남
parseConfig :: String -> Either ParseError Config
-- 문자열을 파싱해서 Config를 반환하거나 에러를 반환
```

### 3. **타입 체크**
```haskell
-- 컴파일 타임에 타입 오류 발견
badFunction :: Int -> String
badFunction x = x + 1  -- 컴파일 에러! Int를 String으로 반환해야 함
```

### 4. **IDE 지원**
```haskell
-- IDE가 자동완성과 타입 정보 제공
processData :: [String] -> [Int]
processData = map read  -- IDE가 타입 정보를 제공
```

## 타입 추론과의 관계

### **타입 추론 예시**
```haskell
-- 타입 선언 없이도 타입 추론 가능
increment x = x + 1  -- Int -> Int로 추론됨

-- 하지만 명시적 선언이 더 좋음
increment :: Int -> Int
increment x = x + 1
```

### **타입 추론 한계**
```haskell
-- 모호한 타입은 추론 불가
ambiguous x = x  -- 어떤 타입인지 모호

-- 명시적 선언 필요
ambiguous :: Int -> Int
ambiguous x = x
```

## 주의사항

### 1. **타입 선언은 선택사항**
```haskell
-- 둘 다 유효함
withType :: Int -> Int
withType x = x + 1

withoutType x = x + 1  -- 타입 추론됨
```

### 2. **타입 선언이 잘못되면 컴파일 에러**
```haskell
-- 잘못된 타입 선언
wrongType :: Int -> String
wrongType x = x + 1  -- Int를 String으로 반환해야 하는데 Int 반환
```

### 3. **타입 변수는 소문자로 시작**
```haskell
-- 올바른 타입 변수
generic :: a -> a

-- 잘못된 타입 변수 (대문자로 시작하면 구체적 타입으로 인식)
-- generic :: A -> A  -- A는 구체적 타입으로 인식됨
```

## 요약

타입 선언 연산자 `::`는:
- **문법**: `식별자 :: 타입`
- **목적**: 함수나 값의 타입을 명시적으로 선언
- **장점**: 명확성, 문서화, 타입 체크, IDE 지원
- **유연성**: 모든 Haskell 타입이 `::` 뒤에 올 수 있음
- **선택사항**: 타입 추론이 가능하지만 명시적 선언이 권장됨

Haskell의 타입 시스템을 활용하는 핵심 도구입니다!
