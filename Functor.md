# Functor
- **함수를 적용할 수 있는 컨텍스트**를 나타내는 type class

## 기본 정의
```hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b 
```
- fmap : Functor의 핵심 함수
  fmap lets you take a function and apply it to a container of type f.
  "fmap은 어떤 함수를 받아서, f 타입의 컨테이너 안에 있는 값에 그 함수를 적용할 수 있게 해준다."
- (a -> b) : 일반적인 함수. 컨테이너 안의 값을 바꾸는 함수.
- f는 컨텍스트(박스)를 나타내는 타입
- container of type f : Maybe, List, IO, Either 등 Functor 타입
```hs
fmap (+1) (Just 3)
fmap (*2) [1, 2, 3]
fmap show (Right 42)
fmap show (getline)
```

```hs
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap _ Nothing = Nothing
```

## Functor 법칙
- 항등 법칙(Identity Law)
- 합성 법칙(Composition Law)
```hs
fmap id = id
fmap ( f . g) = fmap f . fmap g
```

## 중위 연산자 <$>
```hs
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
```

## 동작 원리
```hs
-- 일반 함수
addOne :: Int -> Int
addOne x = x + 1

-- 일반적인 사용
result = addOne 5  -- 6

-- Maybe Functor
result1 = fmap addOne (Just 5)    -- Just 6
result2 = fmap addOne Nothing     -- Nothing

-- List Functor
result3 = fmap addOne [1, 2, 3]   -- [2, 3, 4]

-- Parser Functor
result4 = fmap addOne (parse digit "" "5")  -- Right 6
```

## 주요 Functor 인스턴스들
### Maybe
```hs
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing  = Nothing

-- 사용 예제
fmap (*2) (Just 3)    -- Just 6
fmap (*2) Nothing     -- Nothing
```
### List
```hs
instance Functor [] where
  fmap = map

-- 사용 예제
fmap (*2) [1, 2, 3]   -- [2, 4, 6]
fmap show [1, 2, 3]   -- ["1", "2", "3"]
```
### Parser
```hs
instance Functor (ParsecT s u m) where
  fmap f p = do
    result <- p
    return (f result)

-- 사용 예제
fmap toUpper letter    -- 대문자로 변환된 문자 파서
fmap read (many1 digit) -- 정수로 변환된 숫자 파서
```