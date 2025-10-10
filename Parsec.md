# Parsec
- many
- choice
- try
- sepBy

# 학습 포인트
- Parser a 타입의 의미
- do 블럭을 통한 파서 조합
- 에러 처리 방식

# 📁 프로젝트 구조
Parsec/
├── examples/                    # 13개의 단계별 학습 예제
│   ├── step1-basics/           # 1단계: 기본 개념 (3개 파일)
│   ├── step2-simple/           # 2단계: 간단한 파서 (4개 파일)
│   ├── step3-structured/       # 3단계: 구조화된 데이터 (3개 파일)
│   └── step4-error-handling/   # 4단계: 에러 처리 (3개 파일)
├── src/                        # 라이브러리 소스
├── app/                        # 실행 파일
├── test/                       # 테스트
├── package.yaml               # Stack 프로젝트 설정
└── README.md                  # 상세한 학습 가이드


```shell
cd /home/milman2/haskell/Parsec
stack build

# 1단계: 기본 개념
stack exec -- runhaskell examples/step1-basics/01_ParserType.hs

# 2단계: 간단한 파서
stack exec -- runhaskell examples/step2-simple/01_NumberParsing.hs

# 3단계: 구조화된 데이터
stack exec -- runhaskell examples/step3-structured/01_KeyValueParsing.hs

# 4단계: 에러 처리
stack exec -- runhaskell examples/step4-error-handling/01_Backtracking.hs
```

# Parsec의 주요 연산자들
## 기본 연산자
```hs
-- Functor
(<$>) :: Functor f => (a -> b) -> f a -> f b
-- Applicative
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(<*)  :: Applicative f => f a -> f b -> f a
(*>)  :: Applicative f => f a -> f b -> f b
-- Alternative
(<|>) :: Alternative f => f a -> f a -> f a
```
## 파싱 연산자
```hs
-- 백트래킹
try :: ParsecT s u m a -> ParsecT s u m a
-- 에러 메시지
(<?>) :: ParsecT s u m a -> String -> ParsecT s u m a
-- 미리 보기
lookAhead :: ParsecT s u m a -> ParsecT s u m a
-- 다음에 오지 않음
notFollowedBy :: ParsecT s u m a -> ParsecT s u m ()
```
## 구조화 연산자
```hs
-- 사이에
between :: ParsecT s u m open -> ParsecT s u m close -> ParsecT s u m a -> ParsecT s u m a
-- 구분자로 분리
sepBy :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1 :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
-- 구분자로 끝
endBy :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
-- 선택적 끝
sepEndBy :: ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
```
## 개수 제어 연산자
```hs
-- 정확한 개수
count :: Int -> ParsecT s u m a -> ParsecT s u m [a]
-- 특정 패턴까지
manyTill :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
-- 건너뛰기
skipMany :: ParsecT s u m a -> ParsecT s u m ()
skipMany1 :: ParsecT s u m a -> ParsecT s u m ()
```

# 예제
## 기본 조합
```hs
-- 함수 적용
upperCase = toUpper <$> letter

-- 튜플 생성
pairParser = (,) <$> letter <*> digit

-- 선택
letterOrDigit = letter <|> digit

-- 백트래킹
tryExample = try (string "hello") <|> string "hi"

-- 에러 메시지
customError = string "hello" <?> "expected 'hello'"
```
## 구조화된 파싱싱
```hs
-- 괄호 안의 내용
parentheses = between (char '(') (char ')') (many (noneOf "()"))

-- 쉼표로 구분된 리스트
commaList = sepBy (many1 letter) (char ',')

-- 최소 1개 요소
nonEmptyList = sepBy1 (many1 letter) (char ',')

-- 정확한 개수
exactCount = count 3 (many1 letter)
```
## 복잡한 조합
```hs
-- 함수 호출 파싱
functionCall = do
  name <- many1 letter
  char '('
  args <- sepBy (many1 letter) (char ',')
  char ')'
  return (name, args)

-- 키워드 파싱
keyword kw = do
  result <- try (string kw)
  notFollowedBy (letter <|> digit <|> char '_')
  return result
```