# Haskell 키워드 (Keywords)

Haskell은 함수형 프로그래밍 언어로, 특별한 의미를 가진 예약어들이 있습니다. 이 문서는 Haskell의 주요 키워드들을 분류하고 설명합니다.

## 목차
1. [데이터 타입 관련 키워드](#데이터-타입-관련-키워드)
2. [함수 정의 관련 키워드](#함수-정의-관련-키워드)
3. [제어 구조 키워드](#제어-구조-키워드)
4. [모나드 관련 키워드](#모나드-관련-키워드)
5. [타입 클래스 관련 키워드](#타입-클래스-관련-키워드)
6. [모듈 관련 키워드](#모듈-관련-키워드)
7. [기타 키워드](#기타-키워드)
8. [특수 기호](#특수-기호)

---

## 데이터 타입 관련 키워드

### `data`
새로운 대수적 데이터 타입을 정의합니다.

```haskell
data Color = Red | Green | Blue
data Maybe a = Nothing | Just a
data Tree a = Leaf a | Node (Tree a) (Tree a)
```

### `type`
타입 별칭을 정의합니다.

```haskell
type String = [Char]
type Point = (Int, Int)
type Name = String
```

### `newtype`
새로운 타입을 정의하지만 단일 생성자와 단일 필드만 가질 수 있습니다.

```haskell
newtype Age = Age Int
newtype Email = Email String
```

### `deriving`
자동으로 타입 클래스 인스턴스를 생성합니다.

```haskell
data Color = Red | Green | Blue deriving (Show, Eq, Ord)
data Person = Person String Int deriving (Show)
```

---

## 함수 정의 관련 키워드

### `where`
함수 정의에서 지역 변수나 보조 함수를 정의합니다.

```haskell
factorial n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n-1) (n*acc)
```

### `let`
지역 바인딩을 만듭니다.

```haskell
let x = 10
    y = 20
in x + y
```

### `in`
`let` 표현식에서 바인딩을 사용할 범위를 지정합니다.

```haskell
let square x = x * x
in square 5
```

---

## 제어 구조 키워드

### `if`
조건부 표현식입니다.

```haskell
if x > 0 then "positive" else "negative or zero"
```

### `then`
`if` 표현식의 참일 때 실행할 부분을 지정합니다.

### `else`
`if` 표현식의 거짓일 때 실행할 부분을 지정합니다.

### `case`
패턴 매칭을 수행합니다.

```haskell
case x of
  Nothing -> "No value"
  Just n  -> "Value is " ++ show n
```

### `of`
`case` 표현식에서 패턴을 나열하기 시작합니다.

---

## 모나드 관련 키워드

### `do`
모나드 체이닝을 위한 `do` 표기법을 시작합니다.

```haskell
main = do
  putStrLn "Hello"
  name <- getLine
  putStrLn $ "Hello, " ++ name
```

### `return`
모나드에 값을 래핑합니다.

```haskell
return 42 :: IO Int
return "hello" :: Maybe String
```

### `<-`
모나드에서 값을 추출합니다.

```haskell
do
  x <- getLine
  y <- getLine
  return (x ++ y)
```

---

## 타입 클래스 관련 키워드

### `class`
타입 클래스를 정의합니다.

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

### `instance`
타입 클래스의 인스턴스를 정의합니다.

```haskell
instance Eq Bool where
  True  == True  = True
  False == False = True
  _     == _     = False
```

### `=>`
타입 클래스 제약을 나타냅니다.

```haskell
sort :: Ord a => [a] -> [a]
show :: Show a => a -> String
```

---

## 모듈 관련 키워드

### `module`
모듈을 정의합니다.

```haskell
module MyModule (exportedFunction) where
```

### `import`
다른 모듈을 가져옵니다.

```haskell
import Data.List
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
```

### `qualified`
모듈을 정규화된 이름으로 가져옵니다.

```haskell
import qualified Data.Map as Map
-- 사용: Map.lookup
```

### `as`
모듈에 별칭을 부여합니다.

```haskell
import Data.Map as M
-- 사용: M.lookup
```

### `hiding`
특정 함수들을 가져오지 않습니다.

```haskell
import Data.List hiding (sort)
```

---

## 기타 키워드

### `infix`, `infixl`, `infixr`
연산자의 우선순위와 결합성을 정의합니다.

```haskell
infixl 6 +, -
infixr 5 ++
infix 4 ==, /=
```

### `foreign`
외부 함수 인터페이스(FFI)를 정의합니다.

```haskell
foreign import ccall "math.h sin" c_sin :: Double -> Double
```

### `unsafe`
안전하지 않은 연산을 표시합니다.

```haskell
unsafePerformIO :: IO a -> a
```

### `default`
기본 타입을 지정합니다.

```haskell
default (Int, Double)
```

---

## 특수 기호

### `::`
타입 시그니처를 나타냅니다.

```haskell
factorial :: Int -> Int
map :: (a -> b) -> [a] -> [b]
```

### `->`
함수 타입을 나타냅니다.

```haskell
Int -> String -> Bool
```

### `=`
함수 정의나 값 바인딩에 사용됩니다.

```haskell
x = 42
add x y = x + y
```

### `|`
가드나 데이터 타입의 대안을 나타냅니다.

```haskell
-- 가드
abs x | x >= 0 = x
      | otherwise = -x

-- 데이터 타입
data Maybe a = Nothing | Just a
```

### `_`
와일드카드 패턴입니다.

```haskell
case x of
  (_, y) -> y
  _      -> 0
```

### `@`
패턴에서 전체 값을 바인딩합니다.

```haskell
case xs of
  [] -> []
  x@(y:ys) -> x : process ys
```

### `!`
엄격한 평가를 강제합니다.

```haskell
data StrictPair = StrictPair !Int !String
```

### `~`
지연 평가를 명시합니다.

```haskell
f ~(x, y) = x + y
```

---

## 예약어 목록

Haskell의 완전한 예약어 목록:

```
as, case, class, data, default, deriving, do, else, foreign, 
if, import, in, infix, infixl, infixr, instance, let, module, 
newtype, of, qualified, then, type, where, _
```

## 주의사항

1. **예약어는 변수명으로 사용할 수 없습니다.**
2. **일부 키워드는 특정 컨텍스트에서만 사용됩니다.**
3. **대소문자를 구분합니다.**
4. **특수 기호들은 연산자로 사용될 수 있습니다.**

## 참고 자료

- [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)
- [GHC User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
- [Learn You a Haskell](http://learnyouahaskell.com/)

---

*이 문서는 Haskell의 주요 키워드들을 정리한 것입니다. 더 자세한 내용은 공식 문서를 참조하세요.*
