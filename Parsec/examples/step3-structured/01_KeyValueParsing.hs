{-# LANGUAGE OverloadedStrings #-}
module Step3.KeyValueParsing where

import Text.Parsec
import Text.Parsec.String

-- 3️⃣ 구조화된 데이터 파싱
-- 키-값 쌍 (key=value)

-- 기본 키-값 쌍
keyValue :: Parser (String, String)
keyValue = do
  key <- many1 (letter <|> digit <|> char '_')
  char '='
  value <- many1 (letter <|> digit <|> char '_')
  return (key, value)

-- 공백을 고려한 키-값 쌍
keyValueWithSpaces :: Parser (String, String)
keyValueWithSpaces = do
  spaces
  key <- many1 (letter <|> digit <|> char '_')
  spaces
  char '='
  spaces
  value <- many1 (letter <|> digit <|> char '_')
  spaces
  return (key, value)

-- 따옴표로 감싸진 값
keyValueQuoted :: Parser (String, String)
keyValueQuoted = do
  key <- many1 (letter <|> digit <|> char '_')
  char '='
  char '"'
  value <- many (noneOf "\"")
  char '"'
  return (key, value)

-- 줄바꿈용 키-값 쌍 (공백 처리 없음)
keyValueForNewline :: Parser (String, String)
keyValueForNewline = do
  key <- many1 (letter <|> digit <|> char '_')
  char '='
  value <- many1 (letter <|> digit <|> char '_')
  return (key, value)

-- 여러 키-값 쌍 (줄바꿈으로 구분)
keyValueList :: Parser [(String, String)]
keyValueList = do
  pairs <- sepBy keyValueForNewline (char '\n')
  return pairs

-- 여러 키-값 쌍 (쉼표로 구분)
keyValueCommaList :: Parser [(String, String)]
keyValueCommaList = do
  pairs <- sepBy keyValueWithSpaces (char ',')
  return pairs

-- 중괄호로 감싸진 키-값 쌍들
keyValueBlock :: Parser [(String, String)]
keyValueBlock = do
  char '{'
  spaces
  pairs <- sepBy keyValueWithSpaces (char ',')
  spaces
  char '}'
  return pairs

-- JSON 스타일 키-값 쌍
jsonKeyValue :: Parser (String, String)
jsonKeyValue = do
  spaces
  char '"'
  key <- many (noneOf "\"")
  char '"'
  spaces
  char ':'
  spaces
  char '"'
  value <- many (noneOf "\"")
  char '"'
  spaces
  return (key, value)

-- JSON 스타일 객체
jsonObject :: Parser [(String, String)]
jsonObject = do
  char '{'
  spaces
  pairs <- sepBy jsonKeyValue (char ',')
  spaces
  char '}'
  return pairs

-- 환경변수 스타일 (KEY=VALUE)
envVarStyle :: Parser (String, String)
envVarStyle = do
  key <- many1 (letter <|> digit <|> char '_')
  char '='
  value <- many anyChar
  return (key, value)

main :: IO ()
main = do
  putStrLn "=== 키-값 쌍 파싱 예제 ==="
  
  -- 기본 키-값 쌍
  putStrLn "\n1. 기본 키-값 쌍:"
  parseTest keyValue "name=john"
  parseTest keyValue "age=25"
  
  -- 공백을 고려한 키-값 쌍
  putStrLn "\n2. 공백을 고려한 키-값 쌍:"
  parseTest keyValueWithSpaces "name = john"
  parseTest keyValueWithSpaces "  age  =  25  "
  
  -- 따옴표로 감싸진 값
  putStrLn "\n3. 따옴표로 감싸진 값:"
  parseTest keyValueQuoted "name=\"john doe\""
  parseTest keyValueQuoted "message=\"hello world\""
  
  -- 여러 키-값 쌍 (줄바꿈)
  putStrLn "\n4. 여러 키-값 쌍 (줄바꿈):"
  parseTest keyValueList "name=john\nage=25\ncity=seoul"
  
  -- 여러 키-값 쌍 (쉼표)
  putStrLn "\n5. 여러 키-값 쌍 (쉼표):"
  parseTest keyValueCommaList "name=john,age=25,city=seoul"
  
  -- 중괄호로 감싸진 키-값 쌍들
  putStrLn "\n6. 중괄호로 감싸진 키-값 쌍들:"
  parseTest keyValueBlock "{name=john,age=25,city=seoul}"
  
  -- JSON 스타일 키-값 쌍
  putStrLn "\n7. JSON 스타일 키-값 쌍:"
  parseTest jsonKeyValue "\"name\":\"john\""
  
  -- JSON 스타일 객체
  putStrLn "\n8. JSON 스타일 객체:"
  parseTest jsonObject "{\"name\":\"john\",\"age\":\"25\"}"
  
  -- 환경변수 스타일
  putStrLn "\n9. 환경변수 스타일:"
  parseTest envVarStyle "PATH=/usr/bin:/bin"
  parseTest envVarStyle "HOME=/home/user"
