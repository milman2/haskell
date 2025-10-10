# Applicative
- **함수가 들어 있는 컨텍스트에 값을 적용할 수 있는** 타입 클래스
## 기본 정의
```hs
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
- pure: 값을 컨텍스트로 감싸기 = 값을 컨테이너에 넣는 함수.
- <*>: 함수가 들어있는 컨텍스트에 값을 적용 = 컨테이너 안의 함수와 컨테이너 안의 값을 결합하는 연산자.

- **함수를 컨테이너 안에 넣고**, 또 다른 컨테이너 안의 값에 적용할 수 있게 해주는 추상화
- 병렬적이고 구조적인(독립적인) 계산을 가능하게 함

## 예시
- Maybe, [], IO, Either, ZipList, Parser
```hs
pure (+1) <*> Just 3  -- 결과: Just 4
Just (*2) <*> Just 5  -- 결과: Just 10
Nothing <*> Just 5    -- 결과: Nothing

[(+1), (*2)] <*> [1,2]  -- 결과: [2,3,2,4]
```

```hs
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe a b = pure (+) <*> a <*> b

addMaybe (Just 3) (Just 4)
-- 동작 흐름 예시
-- pure (+) → Just (+)
-- Just (+) <*> Just 3 → Just (3 +) → 결과: Just (\x -> 3 + x)
-- Just (\x -> 3 + x) <*> Just 4 → Just (3 + 4) → Just 7
```

## 응용 분야
### 옵션/설정 값 처리 (Maybe, Either)
- 사용자 입력 폼에서 모든 필드가 유효할 때만 결과 생성
```hs
makeUser :: Maybe String -> Maybe Int -> Maybe User
makeUser name age = User <$> name <*> age
```
### 검증 로직(Validation)
- Monad는 첫 에러에서 멈추지만, Applicative는 모든 에러를 수집할 수 있음
```hs
validateAge :: String -> Either [String] Int
validateAge ageStr =
  if all isDigit ageStr
    then Right (read ageStr)
    else Left ["Invalid age"]
validateName :: String -> Either [String] String
validateName name =
  if null name then Left ["Name is empty"] else Right name

validateUser :: String -> String -> Either [String] User
validateUser name ageStr =
  User <$> validateName name <*> validateAge ageStr
```
### 파서 조합(Parse, Megaparsec)
- Applicative 스타일로 파서를 조합하면 코드가 간결해짐
```hs
data Person = Person String Int

personParser :: Parser Person
personParser = Person <$> nameParser <*> ageParser
```
### 컨테이너 계산 (List, ZipList)
```hs
ZipList [(+1), (*2)] <*> ZipList [10, 20]  -- 결과: ZipList [11, 40]
```
### IO 연산 조합
```hs
greet :: IO ()
greet = putStrLn <$> getLine <*> getLine
```
### 기계 학습, 수학 계산, 그래프 조합 등
