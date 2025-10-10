{-# LANGUAGE OverloadedStrings #-}
module Step2.WhitespaceHandling where

import Text.Parsec
import Text.Parsec.String

-- 2️⃣ 간단한 파서 작성
-- 공백 무시 (spaces, lexeme)

-- spaces: 0개 이상의 공백 문자 (공백, 탭, 개행)
-- lexeme: 파서 뒤에 공백을 무시

-- 기본 공백 처리
basicSpaces :: Parser String
basicSpaces = do
  spaces
  word <- many1 letter
  spaces
  return word

-- lexeme 사용
lexemeWord :: Parser String
lexemeWord = lexeme (many1 letter)

-- 여러 lexeme 조합
lexemePair :: Parser (String, String)
lexemePair = do
  first <- lexeme (many1 letter)
  second <- lexeme (many1 letter)
  return (first, second)

-- lexeme과 일반 파서 조합
mixedParsing :: Parser (String, String)
mixedParsing = do
  first <- lexeme (many1 letter)
  char '='
  spaces
  second <- many1 letter
  return (first, second)

-- skipSpaces: 공백을 소비하지만 결과는 무시
skipSpacesExample :: Parser String
skipSpacesExample = do
  skipSpaces
  word <- many1 letter
  skipSpaces
  return word

-- 공백을 고려한 키워드 파싱
keywordWithSpaces :: String -> Parser String
keywordWithSpaces kw = lexeme (string kw)

-- 공백을 고려한 연산자 파싱
operatorWithSpaces :: String -> Parser String
operatorWithSpaces op = lexeme (string op)

-- 공백을 고려한 식별자 파싱
identifierWithSpaces :: Parser String
identifierWithSpaces = lexeme $ do
  first <- letter
  rest <- many (letter <|> digit <|> char '_')
  return (first : rest)

-- 공백을 고려한 정수 파싱
integerWithSpaces :: Parser Int
integerWithSpaces = lexeme $ do
  digits <- many1 digit
  return (read digits)

-- 공백을 고려한 할당문 파싱
assignmentWithSpaces :: Parser (String, Int)
assignmentWithSpaces = do
  var <- identifierWithSpaces
  operatorWithSpaces "="
  value <- integerWithSpaces
  return (var, value)

main :: IO ()
main = do
  putStrLn "=== 공백 처리 예제 ==="
  
  -- 기본 공백 처리
  putStrLn "\n1. 기본 공백 처리:"
  parseTest basicSpaces "  hello  "
  parseTest basicSpaces "hello"
  
  -- lexeme 사용
  putStrLn "\n2. lexeme 사용:"
  parseTest lexemeWord "hello   "
  parseTest lexemeWord "hello"
  
  -- lexeme 쌍
  putStrLn "\n3. lexeme 쌍:"
  parseTest lexemePair "hello world"
  parseTest lexemePair "hello   world   "
  
  -- 혼합 파싱
  putStrLn "\n4. 혼합 파싱:"
  parseTest mixedParsing "x = 42"
  parseTest mixedParsing "x=42"
  
  -- skipSpaces
  putStrLn "\n5. skipSpaces:"
  parseTest skipSpacesExample "   hello   "
  
  -- 키워드와 공백
  putStrLn "\n6. 키워드와 공백:"
  parseTest (keywordWithSpaces "if") "if   "
  parseTest (keywordWithSpaces "if") "if"
  
  -- 연산자와 공백
  putStrLn "\n7. 연산자와 공백:"
  parseTest (operatorWithSpaces "+") "+   "
  parseTest (operatorWithSpaces "+") "+"
  
  -- 식별자와 공백
  putStrLn "\n8. 식별자와 공백:"
  parseTest identifierWithSpaces "variable   "
  parseTest identifierWithSpaces "variable"
  
  -- 정수와 공백
  putStrLn "\n9. 정수와 공백:"
  parseTest integerWithSpaces "123   "
  parseTest integerWithSpaces "123"
  
  -- 할당문과 공백
  putStrLn "\n10. 할당문과 공백:"
  parseTest assignmentWithSpaces "x = 42"
  parseTest assignmentWithSpaces "x=42"
  parseTest assignmentWithSpaces "x   =   42"
