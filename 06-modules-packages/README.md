# 6단계: 모듈과 패키지 관리

## 학습 목표
- Haskell 모듈 시스템 이해
- 모듈 정의와 import/export
- Cabal과 Stack을 사용한 패키지 관리
- 외부 라이브러리 사용법
- 프로젝트 구조 설계

## 학습 내용

### 1. 모듈 시스템
- 모듈 정의와 네임스페이스
- `module` 키워드와 export/import
- 모듈 계층 구조

### 2. 패키지 관리
- Cabal 프로젝트 설정
- Stack을 사용한 의존성 관리
- 버전 관리와 호환성

### 3. 외부 라이브러리
- Hackage에서 패키지 찾기
- 의존성 추가와 관리
- 버전 제약 조건

### 4. 프로젝트 구조
- 라이브러리와 실행 파일 분리
- 테스트 모듈 구성
- 문서화

## 프로젝트: 라이브러리 프로젝트와 패키지 의존성

### 구현할 기능
1. 수학 유틸리티 라이브러리
2. JSON 파싱 도구
3. HTTP 클라이언트 라이브러리
4. 데이터베이스 연결 모듈

### 예제 코드

#### MathUtils.hs (수학 유틸리티 모듈)
```haskell
module MathUtils
    ( factorial
    , fibonacci
    , isPrime
    , gcd
    , lcm
    , Statistics(..)
    , calculateStats
    ) where

-- 팩토리얼 계산
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 피보나치 수열
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- 소수 판별
isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [3,5..(floor $ sqrt $ fromIntegral n)]

-- 최대공약수
gcd :: Integer -> Integer -> Integer
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

-- 최소공배수
lcm :: Integer -> Integer -> Integer
lcm a b = (a * b) `div` (gcd a b)

-- 통계 데이터 타입
data Statistics = Statistics
    { mean :: Double
    , median :: Double
    , mode :: Maybe Integer
    , standardDeviation :: Double
    } deriving (Show, Eq)

-- 통계 계산
calculateStats :: [Integer] -> Statistics
calculateStats [] = Statistics 0 0 Nothing 0
calculateStats xs = Statistics
    { mean = fromIntegral (sum xs) / fromIntegral (length xs)
    , median = calculateMedian xs
    , mode = calculateMode xs
    , standardDeviation = calculateStdDev xs
    }

calculateMedian :: [Integer] -> Double
calculateMedian xs = 
    let sorted = sort xs
        len = length sorted
    in if even len
        then fromIntegral (sorted !! (len `div` 2 - 1) + sorted !! (len `div` 2)) / 2
        else fromIntegral (sorted !! (len `div` 2))

calculateMode :: [Integer] -> Maybe Integer
calculateMode xs = 
    let counts = map (\x -> (x, length $ filter (== x) xs)) (nub xs)
        maxCount = maximum $ map snd counts
    in if maxCount == 1 then Nothing else Just (fst $ head $ filter ((== maxCount) . snd) counts)

calculateStdDev :: [Integer] -> Double
calculateStdDev xs = 
    let avg = fromIntegral (sum xs) / fromIntegral (length xs)
        variance = sum $ map (\x -> (fromIntegral x - avg) ^ 2) xs
    in sqrt (variance / fromIntegral (length xs))
```

#### JSONParser.hs (JSON 파싱 모듈)
```haskell
module JSONParser
    ( JSONValue(..)
    , parseJSON
    , toJSON
    , getString
    , getNumber
    , getArray
    , getObject
    ) where

import Data.Char
import Data.List

-- JSON 값 타입 정의
data JSONValue = JSONString String
               | JSONNumber Double
               | JSONBool Bool
               | JSONNull
               | JSONArray [JSONValue]
               | JSONObject [(String, JSONValue)]
               deriving (Show, Eq)

-- JSON 파싱 함수
parseJSON :: String -> Maybe JSONValue
parseJSON input = case parseValue (trim input) of
    (Just value, "") -> Just value
    _ -> Nothing

parseValue :: String -> (Maybe JSONValue, String)
parseValue ('"':rest) = parseString rest
parseValue ('t':rest) = if take 4 rest == "rue" then (Just $ JSONBool True, drop 4 rest) else (Nothing, rest)
parseValue ('f':rest) = if take 5 rest == "alse" then (Just $ JSONBool False, drop 5 rest) else (Nothing, rest)
parseValue ('n':rest) = if take 4 rest == "ull" then (Just JSONNull, drop 4 rest) else (Nothing, rest)
parseValue ('[':rest) = parseArray rest
parseValue ('{':rest) = parseObject rest
parseValue input = parseNumber input

parseString :: String -> (Maybe JSONValue, String)
parseString input = 
    let (str, rest) = span (/= '"') input
    in if head rest == '"' then (Just $ JSONString str, tail rest) else (Nothing, input)

parseNumber :: String -> (Maybe JSONValue, String)
parseNumber input = 
    let (numStr, rest) = span (\c -> isDigit c || c == '.' || c == '-' || c == 'e' || c == 'E') input
    in case reads numStr of
        [(num, "")] -> (Just $ JSONNumber num, rest)
        _ -> (Nothing, input)

parseArray :: String -> (Maybe JSONValue, String)
parseArray input = 
    let trimmed = trim input
    in if head trimmed == ']' then (Just $ JSONArray [], tail trimmed) else parseArrayElements trimmed

parseArrayElements :: String -> (Maybe JSONValue, String)
parseArrayElements input = 
    let (first, rest1) = parseValue input
        trimmed = trim rest1
    in case first of
        Nothing -> (Nothing, input)
        Just value -> if head trimmed == ']' 
            then (Just $ JSONArray [value], tail trimmed)
            else if head trimmed == ','
                then case parseArrayElements (tail trimmed) of
                    (Just (JSONArray rest), finalRest) -> (Just $ JSONArray (value:rest), finalRest)
                    _ -> (Nothing, input)
                else (Nothing, input)

parseObject :: String -> (Maybe JSONValue, String)
parseObject input = 
    let trimmed = trim input
    in if head trimmed == '}' then (Just $ JSONObject [], tail trimmed) else parseObjectElements trimmed

parseObjectElements :: String -> (Maybe JSONValue, String)
parseObjectElements input = 
    let (key, rest1) = parseString input
        trimmed1 = trim rest1
    in case key of
        Nothing -> (Nothing, input)
        Just (JSONString keyStr) -> 
            if head trimmed1 == ':'
                then let (value, rest2) = parseValue (tail trimmed1)
                         trimmed2 = trim rest2
                     in case value of
                        Nothing -> (Nothing, input)
                        Just val -> if head trimmed2 == '}'
                            then (Just $ JSONObject [(keyStr, val)], tail trimmed2)
                            else if head trimmed2 == ','
                                then case parseObjectElements (tail trimmed2) of
                                    (Just (JSONObject rest), finalRest) -> (Just $ JSONObject ((keyStr, val):rest), finalRest)
                                    _ -> (Nothing, input)
                                else (Nothing, input)
                else (Nothing, input)
        _ -> (Nothing, input)

-- 유틸리티 함수들
trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- JSON 값 접근 함수들
getString :: JSONValue -> Maybe String
getString (JSONString s) = Just s
getString _ = Nothing

getNumber :: JSONValue -> Maybe Double
getNumber (JSONNumber n) = Just n
getNumber _ = Nothing

getArray :: JSONValue -> Maybe [JSONValue]
getArray (JSONArray a) = Just a
getArray _ = Nothing

getObject :: JSONValue -> Maybe [(String, JSONValue)]
getObject (JSONObject o) = Just o
getObject _ = Nothing
```

#### package.yaml (Stack 프로젝트 설정)
```yaml
name: haskell-learning
version: 0.1.0.0
description: Haskell learning project
author: Your Name
maintainer: your.email@example.com
copyright: 2024
license: MIT
build-type: Simple
cabal-version: >=1.10

library
  exposed-modules:
    - MathUtils
    - JSONParser
  build-depends:
    - base >=4.7 && <5
    - text
    - aeson
    - http-conduit
    - bytestring

executables:
  math-calculator:
    main: Main.hs
    source-dirs: src
    build-depends:
      - base
      - haskell-learning
    ghc-options:
      - -threaded
      - -rtsopts
      - -with-rtsopts=-N

tests:
  test-suite:
    main: Spec.hs
    source-dirs: test
    build-depends:
      - base
      - haskell-learning
      - hspec
```

## 연습 문제
1. 자신만의 문자열 처리 라이브러리를 만들어보세요
2. 간단한 설정 파일 파서를 작성하세요
3. 로그 파일 분석 라이브러리를 만들어보세요
4. 외부 API를 호출하는 HTTP 클라이언트를 작성하세요

## 고급 연습 문제
1. 플러그인 시스템을 가진 애플리케이션을 만들어보세요
2. 마이크로서비스 아키텍처를 구현해보세요
3. 실시간 데이터 처리 파이프라인을 만들어보세요

## 테스트 방법
```bash
# Stack 프로젝트 초기화
stack new my-project
cd my-project

# 의존성 설치
stack build

# 테스트 실행
stack test

# 실행 파일 빌드
stack build --exec my-project
```

## 다음 단계
7단계에서는 고급 타입 시스템에 대해 학습합니다.
