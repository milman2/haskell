# Monoid
- **데이터를 결합하고 초기값을 정의하는 방식**을 표현
```hs
class Semigroup m => Monoid m where
  mempty  :: m -- 항등원 (결합에 영향을 주지 않는 기본값). unit = identity element
  mappend :: m -> m -> m  -- 결합 연산 (요즘은 `(<>)`로 대체됨)
  mappend = (<>)
```

## 예시로 이해하기
| 타입         | mempty   | (<>)        |
|--------------|----------|-------------|
| [] (리스트)  | []       | 리스트 연결 |
| String       | ""       | 문자열 연결 |
| Sum Int      | 0        | 덧셈        |
| Product Int  | 1        | 곱셈        |
| Maybe a      | Nothing  | 우선순위 결합 |
| IO ()        | return ()| IO 결합     |

## 실전
- foldMap, mconcat
- 리스트 축약, 로그 누적, 파서 조합, 설정 병합
- 

## Monoid Laws
- Identity Law
- Associative Law
- Closure Law

```hs
newtype MyString = MyString String deriving (Show, Eq)
instance Semigroup MyString where
    MyString x <> Mystring y = MyString(x ++ y)

instance Monoid MyString where
    mempty = MyString ""

main :: IO ()
main = do
    let s1 = MyString "Hello,"
    let s2 = MyString "World,"
    let s3 = MyString ""
    -- mappend (<>)
    print (s1 <> s2)
    -- Test the identity property
    print (s1 <> mempty)
    print (mempty <> s2)
    -- Test assoicativity
    print ((s1 <> s2) <> s3)
    print (s1 <> (s2 <> s3))
```