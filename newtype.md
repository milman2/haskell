# State Monad
```hs
import Control.Monad.State
-- 기본 제공 함수 사용
-- get :: State ss
-- put :: s -> State s ()
-- modify :: (s -> s) -> State s ()
-- gets :: (s -> a) -> State s a

increment :: State Int String
increment = do
    n <- get
    put (n+1)
    return ("Increaed to " ++ show (n + 1))

-- runState increment 10

program :: State Int String
program = do
    increment -- State Monad에서는 상태를 자동으로 전달함.
    increment
    n <- get
    return ("Final state: " ++ show n)

-- runState program 0
```

## 직접 정의 - newtype
```hs
newtype State s a = State { runState :: s -> (a, s)}

-- State 모나드 인스턴스 정의
instance Functor (State s) where
    fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    State f <*> State g = State $ \s -> let (h, s') = f s; (a, s'') = g s' in (h a, s'')

instance Monad (State s) where
    return = pure
    State f >>= g = State $ \s -> let (a, s') = f s in runState (g a) s'

-- State 모나드 유틸리티 함수들
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- 사용 예시
increment :: State Int Int
increment = do
    n <- get
    put (n + 1)
    return n

-- 실행
-- runState increment 0  -- (0, 1)
```

# newtype 정리

## newtype이란?

`newtype`은 **기존 타입에 새로운 이름을 부여**하는 Haskell의 키워드입니다. **단일 생성자, 단일 필드**를 가진 타입만 정의할 수 있습니다.

## 타입 정의 키워드 비교

### **1. `data` - 완전한 새로운 타입**
```haskell
-- 여러 생성자, 여러 필드 가능
data Person = Person String Int
data Color = Red | Green | Blue
data Tree a = Leaf a | Node (Tree a) (Tree a)
```

### **2. `newtype` - 기존 타입의 래퍼**
```haskell
-- 단일 생성자, 단일 필드만 가능
newtype State s a = State { runState :: s -> (a, s) }
newtype Counter = Counter Int
newtype Username = Username String
```

### **3. `type` - 타입 별칭**
```haskell
-- 완전히 동일한 타입, 컴파일 시 교체됨
type String = [Char]
type Point = (Int, Int)
type Stack = [Int]
```

### **4. `class` - 타입 클래스 정의**
```haskell
-- 타입 클래스 정의
class Show a where
    show :: a -> String
```

### **5. `instance` - 타입 클래스 인스턴스**
```haskell
-- 타입 클래스 구현
instance Show Int where
    show = showInt
```

## newtype의 특징

### **장점**
1. **성능**: 런타임 오버헤드 없음 (컴파일 시 제거됨)
2. **타입 안전성**: 기존 타입과 구분하여 사용
3. **의미적 명확성**: 도메인 특화된 타입 생성

### **제한사항**
1. **단일 생성자만 가능**
2. **단일 필드만 가능**
3. **지연 평가**: `!` strictness annotation 사용 불가

## 사용 예시

### **타입 안전성**
```haskell
-- 잘못된 예시
addAge :: Int -> Int -> Int
addAge age years = age + years

-- 올바른 예시
newtype Age = Age Int
newtype Years = Years Int

addAge :: Age -> Years -> Age
addAge (Age age) (Years years) = Age (age + years)
```

### **의미적 명확성**
```haskell
newtype Username = Username String
newtype Password = Password String

login :: Username -> Password -> Bool
login (Username user) (Password pass) = ...
```

### **모나드 래핑**
```haskell
newtype State s a = State { runState :: s -> (a, s) }
newtype Reader r a = Reader { runReader :: r -> a }
newtype Writer w a = Writer { runWriter :: (a, w) }
```

## 언제 사용하나?

### **newtype 사용**
- 기존 타입에 새로운 의미 부여
- 타입 안전성 향상
- 모나드나 펑터 래핑
- 성능이 중요한 경우

### **data 사용**
- 복잡한 데이터 구조
- 여러 생성자 필요
- 여러 필드 필요

### **type 사용**
- 단순한 타입 별칭
- 가독성 향상
- 긴 타입명 단축

## 요약

| 키워드 | 용도 | 생성자 | 필드 | 성능 |
|--------|------|--------|------|------|
| `data` | 새로운 타입 | 여러개 | 여러개 | 오버헤드 있음 |
| `newtype` | 타입 래핑 | 1개 | 1개 | 오버헤드 없음 |
| `type` | 타입 별칭 | - | - | 오버헤드 없음 |
| `class` | 타입 클래스 | - | - | - |
| `instance` | 클래스 구현 | - | - | - |

**newtype은 기존 타입을 안전하고 효율적으로 래핑하는 도구입니다!**
