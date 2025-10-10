# Haskell Type Classes (타입 클래스)

## Type Classes란?

Type Classes는 Haskell의 **다형성(polymorphism)**을 구현하는 핵심 메커니즘입니다. 비슷한 동작을 하는 타입들을 그룹화하여 공통 인터페이스를 제공합니다.

## 기본 개념

### 1. **타입 클래스 정의**

```haskell
-- 기본적인 타입 클래스 정의
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    
    -- 기본 구현 제공
    x /= y = not (x == y)
```

### 2. **인스턴스 정의**

```haskell
-- Int 타입에 대한 Eq 인스턴스
instance Eq Int where
    x == y = x `primEqInt` y

-- Bool 타입에 대한 Eq 인스턴스
instance Eq Bool where
    True  == True  = True
    False == False = True
    _     == _     = False
```

## 주요 타입 클래스들

### 1. **Eq (Equality)**

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-- 사용 예시
isEqual :: Eq a => a -> a -> Bool
isEqual x y = x == y

-- 테스트
testEq = isEqual 5 5        -- True
testEq2 = isEqual "hello" "world"  -- False
```

### 2. **Ord (Ordering)**

```haskell
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    max :: a -> a -> a
    min :: a -> a -> a

-- 사용 예시
sortList :: Ord a => [a] -> [a]
sortList [] = []
sortList (x:xs) = sortList [y | y <- xs, y <= x] ++ [x] ++ sortList [y | y <- xs, y > x]
```

### 3. **Show (String Representation)**

```haskell
class Show a where
    show :: a -> String
    showList :: [a] -> ShowS

-- 사용 예시
display :: Show a => a -> String
display x = "Value: " ++ show x

-- 테스트
testShow = display 42        -- "Value: 42"
testShow2 = display True     -- "Value: True"
```

### 4. **Read (Parsing from String)**

```haskell
class Read a where
    read :: String -> a
    readList :: ReadS [a]

-- 사용 예시
parseInt :: String -> Int
parseInt s = read s

-- 테스트
testRead = parseInt "123"    -- 123
```

### 5. **Num (Numeric Operations)**

```haskell
class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    negate :: a -> a
    abs :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a

-- 사용 예시
double :: Num a => a -> a
double x = x + x

-- 테스트
testNum = double 5          -- 10
testNum2 = double 3.14      -- 6.28
```

### 6. **Functor (Mappable)**

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Maybe에 대한 Functor 인스턴스
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

-- 사용 예시
incrementMaybe :: Maybe Int -> Maybe Int
incrementMaybe = fmap (+1)

-- 테스트
testFunctor = incrementMaybe (Just 5)  -- Just 6
testFunctor2 = incrementMaybe Nothing  -- Nothing
```

### 7. **Monad (Sequential Computation)**

```haskell
class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
    (>>) :: m a -> m b -> m b
    fail :: String -> m a

-- Maybe에 대한 Monad 인스턴스
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x

-- 사용 예시
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

safeCalc :: Int -> Int -> Maybe Int
safeCalc x y = do
    result1 <- safeDiv x y
    result2 <- safeDiv result1 2
    return result2
```

## 타입 클래스 제약 (Type Class Constraints)

### 1. **단일 제약**

```haskell
-- Eq 제약이 있는 함수
isSame :: Eq a => a -> a -> Bool
isSame x y = x == y

-- Ord 제약이 있는 함수
maxValue :: Ord a => a -> a -> a
maxValue x y = if x > y then x else y
```

### 2. **다중 제약**

```haskell
-- 여러 제약이 있는 함수
compareAndShow :: (Eq a, Show a) => a -> a -> String
compareAndShow x y = if x == y 
    then "Same: " ++ show x
    else "Different: " ++ show x ++ " vs " ++ show y

-- Num과 Show 제약
calculateAndShow :: (Num a, Show a) => a -> a -> String
calculateAndShow x y = "Result: " ++ show (x + y)
```

### 3. **제약 체인**

```haskell
-- Ord는 Eq를 상속받음
sortAndCompare :: Ord a => a -> a -> a -> String
sortAndCompare x y z = 
    let sorted = sort [x, y, z]
    in "Sorted: " ++ show sorted
```

## 커스텀 타입 클래스

### 1. **간단한 타입 클래스**

```haskell
-- 크기 비교 가능한 타입 클래스
class Sizeable a where
    size :: a -> Int

-- 인스턴스 정의
instance Sizeable [a] where
    size = length

instance Sizeable String where
    size = length

-- 사용
getSize :: Sizeable a => a -> Int
getSize x = size x
```

### 2. **제약이 있는 타입 클래스**

```haskell
-- 비교 가능한 크기
class (Eq a, Show a) => Sizeable a where
    size :: a -> Int
    isLarger :: a -> a -> Bool
    isLarger x y = size x > size y
```

## 타입 클래스의 장점

### 1. **다형성**
```haskell
-- 하나의 함수로 여러 타입 처리
showValue :: Show a => a -> String
showValue x = show x

-- 다양한 타입에 사용 가능
test1 = showValue 42        -- "42"
test2 = showValue True      -- "True"
test3 = showValue [1,2,3]   -- "[1,2,3]"
```

### 2. **타입 안전성**
```haskell
-- 컴파일 타임에 타입 체크
safeCompare :: Eq a => a -> a -> Bool
safeCompare x y = x == y

-- 잘못된 사용은 컴파일 에러
-- safeCompare 5 "hello"  -- 컴파일 에러!
```

### 3. **코드 재사용**
```haskell
-- 제네릭 정렬 함수
genericSort :: Ord a => [a] -> [a]
genericSort [] = []
genericSort (x:xs) = 
    genericSort [y | y <- xs, y <= x] ++ 
    [x] ++ 
    genericSort [y | y <- xs, y > x]

-- 다양한 타입에 사용
sortedInts = genericSort [3,1,4,1,5]      -- [1,1,3,4,5]
sortedStrings = genericSort ["c","a","b"]  -- ["a","b","c"]
```

## 실제 사용 예시

### 1. **JSON 직렬화**

```haskell
class ToJSON a where
    toJSON :: a -> JSON

instance ToJSON Int where
    toJSON x = JSONNumber x

instance ToJSON String where
    toJSON s = JSONString s
```

### 2. **데이터베이스 연산**

```haskell
class DatabaseEntity a where
    tableName :: a -> String
    primaryKey :: a -> String
    toRow :: a -> [String]
    fromRow :: [String] -> Maybe a
```

## 타입 클래스 계층 구조

```
Show
  ↑
Read
  ↑
Eq
  ↑
Ord
  ↑
Num
  ↑
Real
  ↑
Integral (Int, Integer)
Fractional (Float, Double)
```

## 요약

Type Classes는:
- **인터페이스**: 비슷한 동작을 하는 타입들의 공통 인터페이스
- **다형성**: 하나의 함수로 여러 타입 처리
- **타입 안전성**: 컴파일 타임에 타입 체크
- **확장성**: 새로운 타입에 기존 함수 적용 가능
- **제약**: 타입에 필요한 조건 명시

Haskell의 타입 시스템의 핵심이며, 안전하고 재사용 가능한 코드를 작성하는 데 필수적입니다!

## Haskell Type Class vs C++ Concept

Haskell의 Type Class와 C++의 Concept는 매우 유사한 개념입니다. 둘 다 **다형성(polymorphism)**을 구현하는 메커니즘이며, 타입에 대한 제약을 정의하는 방법입니다.

### 공통점

#### 1. **타입 제약 정의**
```haskell
-- Haskell Type Class
class Show a where
    show :: a -> String
```

```cpp
// C++ Concept
template<typename T>
concept Showable = requires(T t) {
    { show(t) } -> std::convertible_to<std::string>;
};
```

#### 2. **제네릭 프로그래밍**
```haskell
-- Haskell
display :: Show a => a -> String
display x = "Value: " ++ show x
```

```cpp
// C++20
template<Showable T>
std::string display(T x) {
    return "Value: " + show(x);
}
```

#### 3. **컴파일 타임 타입 체크**
```haskell
-- Haskell: 컴파일 타임에 Show 인스턴스 확인
test = display 42  -- OK: Int는 Show 인스턴스
-- test = display (id :: Int -> Int)  -- 컴파일 에러!
```

```cpp
// C++: 컴파일 타임에 Concept 만족 확인
test = display(42);  // OK: int는 Showable concept 만족
// test = display([](int x) { return x; });  // 컴파일 에러!
```

### 차이점

#### 1. **인스턴스 정의 방식**

**Haskell Type Class**
```haskell
data Color = Red | Green | Blue

instance Show Color where
    show Red   = "빨간색"
    show Green = "초록색" 
    show Blue  = "파란색"
    
instance Show Bool where
    show True = "True"
    show False = "False"
```

**C++ Concept**
```cpp
// Concept는 타입에 대한 요구사항만 정의
template<typename T>
concept Showable = requires(T t) {
    { show(t) } -> std::convertible_to<std::string>;
};

// 실제 구현은 별도 함수로
std::string show(int x) { return std::to_string(x); }
std::string show(bool b) { return b ? "true" : "false"; }
```

#### 2. **상속과 제약**

**Haskell Type Class**
```haskell
-- 타입 클래스 상속
class Eq a where
    (==) :: a -> a -> Bool

class (Eq a) => Ord a where
    compare :: a -> a -> Ordering
```

**C++ Concept**
```cpp
// Concept 조합
template<typename T>
concept EqualityComparable = requires(T a, T b) {
    { a == b } -> std::convertible_to<bool>;
};

template<typename T>
concept Ordered = EqualityComparable<T> && requires(T a, T b) {
    { a < b } -> std::convertible_to<bool>;
};
```

#### 3. **사용 문법**

**Haskell**
```haskell
-- 타입 시그니처에서 제약 명시
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]
```

**C++20**
```cpp
// 템플릿에서 concept 사용
template<Ordered T>
std::vector<T> sort(std::vector<T> vec) {
    std::sort(vec.begin(), vec.end());
    return vec;
}
```

### 실제 비교 예시

#### 1. **간단한 비교 가능한 타입**

**Haskell**
```haskell
class Comparable a where
    compare :: a -> a -> Ordering

instance Comparable Int where
    compare x y
        | x < y = LT
        | x > y = GT
        | otherwise = EQ

-- 사용
maxValue :: Comparable a => a -> a -> a
maxValue x y = case compare x y of
    GT -> x
    _  -> y
```

**C++20**
```cpp
template<typename T>
concept Comparable = requires(T a, T b) {
    { a < b } -> std::convertible_to<bool>;
    { a > b } -> std::convertible_to<bool>;
    { a == b } -> std::convertible_to<bool>;
};

template<Comparable T>
T maxValue(T a, T b) {
    return (a > b) ? a : b;
}
```

#### 2. **복잡한 제약**

**Haskell**
```haskell
class (Show a, Eq a) => Displayable a where
    display :: a -> String
    display x = show x

-- 사용
printIfEqual :: Displayable a => a -> a -> IO ()
printIfEqual x y = 
    if x == y
        then putStrLn $ "Equal: " ++ display x
        else putStrLn "Not equal"
```

**C++20**
```cpp
template<typename T>
concept Displayable = requires(T a, T b) {
    { show(a) } -> std::convertible_to<std::string>;
    { a == b } -> std::convertible_to<bool>;
};

template<Displayable T>
void printIfEqual(T a, T b) {
    if (a == b) {
        std::cout << "Equal: " << show(a) << std::endl;
    } else {
        std::cout << "Not equal" << std::endl;
    }
}
```

### 역사적 배경

#### **Haskell Type Class (1987)**
- Haskell 1.0에서 도입
- 함수형 프로그래밍의 다형성 문제 해결
- "ad-hoc polymorphism" 구현

#### **C++ Concept (C++20)**
- 2020년 C++20에서 도입
- 템플릿 메타프로그래밍의 복잡성 해결
- "constrained generics" 구현

### 장단점 비교

#### **Haskell Type Class**
**장점:**
- 더 간결하고 읽기 쉬운 문법
- 강력한 타입 추론
- 인스턴스 자동 유도

**단점:**
- 런타임 오버헤드 (인스턴스 딕셔너리)
- 복잡한 타입 클래스 계층

#### **C++ Concept**
**장점:**
- 컴파일 타임에만 존재 (런타임 오버헤드 없음)
- 템플릿 인스턴스화 제어
- 더 나은 에러 메시지

**단점:**
- 복잡한 문법
- 템플릿 메타프로그래밍 지식 필요

### 요약

Haskell의 Type Class와 C++의 Concept는:

**공통점:**
- 타입에 대한 제약 정의
- 제네릭 프로그래밍 지원
- 컴파일 타임 타입 체크
- 다형성 구현

**차이점:**
- **구현 방식**: Haskell은 인스턴스 정의, C++은 요구사항 정의
- **상속**: Haskell은 클래스 상속, C++은 concept 조합
- **문법**: Haskell이 더 간결, C++이 더 명시적
- **성능**: C++이 런타임 오버헤드 없음

둘 다 **"타입에 대한 인터페이스"**를 정의하는 방법이며, 안전하고 재사용 가능한 제네릭 코드를 작성하는 데 필수적인 도구입니다!

## 참고 자료

- [Haskell Type Classes 공식 문서](https://www.haskell.org/tutorial/classes.html)
- [Learn You a Haskell - Type Classes](http://learnyouahaskell.com/types-and-typeclasses)
- [Real World Haskell - Type Classes](http://book.realworldhaskell.org/read/using-typeclasses.html)
- [C++20 Concepts 공식 문서](https://en.cppreference.com/w/cpp/concepts)
- [Haskell vs C++ Concepts 비교](https://wiki.haskell.org/Typeclassopedia)
