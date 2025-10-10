{-# LANGUAGE OverloadedStrings #-}
module Step2.StringParsing where

import Text.Parsec
import Text.Parsec.String

-- 2️⃣ 간단한 파서 작성
-- 문자, 문자열 파싱

-- 알파벳만 파싱
alphaString :: Parser String
alphaString = many1 letter

-- 알파벳과 숫자 조합 파싱
alphanumericString :: Parser String
alphanumericString = many1 (letter <|> digit)

-- 특정 문자로 구분된 문자열
quotedString :: Parser String
quotedString = do
  char '"'
  content <- many (noneOf "\"")
  char '"'
  return content

-- 백틱으로 감싸진 문자열
backtickString :: Parser String
backtickString = do
  char '`'
  content <- many (noneOf "`")
  char '`'
  return content

-- 식별자 파싱 (첫 글자는 알파벳, 나머지는 알파벳+숫자+언더스코어)
identifier :: Parser String
identifier = do
  first <- letter
  rest <- many (letter <|> digit <|> char '_')
  return (first : rest)

-- 키워드 파싱
keyword :: String -> Parser String
keyword kw = do
  result <- string kw
  notFollowedBy (letter <|> digit <|> char '_')
  return result

-- 공백을 무시하는 따옴표 문자열 파싱
spacedString :: Parser String
spacedString = do
  spaces
  char '"'
  content <- many (noneOf "\"")
  char '"'
  spaces
  return content

-- 이스케이프 문자가 있는 문자열 파싱
escapedString :: Parser String
escapedString = do
  char '"'
  content <- many (escapedChar <|> noneOf "\"")
  char '"'
  return content
  where
    escapedChar = do
      char '\\'
      choice [char '"' >> return '"',
              char '\\' >> return '\\',
              char 'n' >> return '\n',
              char 't' >> return '\t']

main :: IO ()
main = do
  putStrLn "=== 문자열 파싱 예제 ==="
  
  -- 알파벳 문자열
  putStrLn "\n1. 알파벳 문자열 파싱:"
  parseTest alphaString "hello"
  parseTest alphaString "hello123"  -- 실패
  
  -- 알파벳+숫자 문자열
  putStrLn "\n2. 알파벳+숫자 문자열 파싱:"
  parseTest alphanumericString "hello123"
  parseTest alphanumericString "abc"
  parseTest alphanumericString "123"
  
  -- 따옴표 문자열
  putStrLn "\n3. 따옴표 문자열 파싱:"
  parseTest quotedString "\"hello world\""
  parseTest quotedString "\"\""
  
  -- 백틱 문자열
  putStrLn "\n4. 백틱 문자열 파싱:"
  parseTest backtickString "`code here`"
  
  -- 식별자
  putStrLn "\n5. 식별자 파싱:"
  parseTest identifier "variable_name"
  parseTest identifier "var123"
  parseTest identifier "123var"  -- 실패
  
  -- 키워드
  putStrLn "\n6. 키워드 파싱:"
  parseTest (keyword "if") "if"
  parseTest (keyword "if") "ifelse"  -- 실패
  
  -- 공백을 무시하는 따옴표 문자열
  putStrLn "\n7. 공백을 무시하는 따옴표 문자열 파싱:"
  parseTest spacedString "  \"hello world\"  "
  parseTest spacedString "\"hello world\""
  parseTest spacedString "   \"\"   "
  
  -- 이스케이프 문자열
  putStrLn "\n8. 이스케이프 문자열 파싱:"
  parseTest escapedString "\"hello\\nworld\""
  parseTest escapedString "\"quote\\\"here\""
