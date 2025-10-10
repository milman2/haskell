{-# LANGUAGE OverloadedStrings #-}
module Step2.ListParsing where

import Text.Parsec
import Text.Parsec.String

-- 2️⃣ 간단한 파서 작성
-- 리스트, 구분자(sepBy) 처리

-- 쉼표로 구분된 정수 리스트
commaSeparatedInts :: Parser [Int]
commaSeparatedInts = do
  ints <- sepBy intParser (char ',')
  return ints
  where
    intParser = do
      digits <- many1 digit
      return (read digits)

-- 공백으로 구분된 문자열 리스트
spaceSeparatedStrings :: Parser [String]
spaceSeparatedStrings = do
  strings <- sepBy1 (many1 letter) spaces
  return strings

-- 세미콜론으로 구분된 문자열 리스트
semicolonSeparatedStrings :: Parser [String]
semicolonSeparatedStrings = do
  strings <- sepBy (many1 letter) (char ';')
  return strings

-- 괄호로 감싸진 리스트
bracketedList :: Parser [String]
bracketedList = do
  char '['
  strings <- sepBy (many1 letter) (char ',')
  char ']'
  return strings

-- 중괄호로 감싸진 리스트
bracedList :: Parser [String]
bracedList = do
  char '{'
  strings <- sepBy (many1 letter) (char ',')
  char '}'
  return strings

-- sepBy1: 최소 1개 요소가 있는 리스트
nonEmptyList :: Parser [String]
nonEmptyList = do
  char '['
  strings <- sepBy1 (many1 letter) (char ',')
  char ']'
  return strings

-- endBy: 구분자로 끝나는 리스트
endByList :: Parser [String]
endByList = do
  strings <- endBy (many1 letter) (char ',')
  return strings

-- sepEndBy: 구분자로 구분되고 선택적으로 끝나는 리스트
sepEndByList :: Parser [String]
sepEndByList = do
  strings <- sepEndBy (many1 letter) (char ',')
  return strings

-- count: 정확한 개수의 요소
exactCountList :: Parser [Char]
exactCountList = do
  chars <- count 3 letter
  return chars

-- between: 특정 문자로 감싸진 리스트
betweenList :: Parser [String]
betweenList = do
  strings <- between (char '(') (char ')') (sepBy (many1 letter) (char ','))
  return strings

main :: IO ()
main = do
  putStrLn "=== 리스트 파싱 예제 ==="
  
  -- 쉼표로 구분된 정수
  putStrLn "\n1. 쉼표로 구분된 정수 리스트:"
  parseTest commaSeparatedInts "1,2,3,4"
  parseTest commaSeparatedInts "1"
  parseTest commaSeparatedInts ""
  
  -- 공백으로 구분된 문자열
  putStrLn "\n2. 공백으로 구분된 문자열 리스트:"
  parseTest spaceSeparatedStrings "hello world test"
  
  -- 세미콜론으로 구분된 문자열
  putStrLn "\n3. 세미콜론으로 구분된 문자열 리스트:"
  parseTest semicolonSeparatedStrings "a;b;c"
  parseTest semicolonSeparatedStrings "a"
  
  -- 괄호로 감싸진 리스트
  putStrLn "\n4. 괄호로 감싸진 리스트:"
  parseTest bracketedList "[a,b,c]"
  parseTest bracketedList "[]"
  
  -- 중괄호로 감싸진 리스트
  putStrLn "\n5. 중괄호로 감싸진 리스트:"
  parseTest bracedList "{a,b,c}"
  
  -- sepBy1 (최소 1개)
  putStrLn "\n6. sepBy1 (최소 1개 요소):"
  parseTest nonEmptyList "[a,b,c]"
  parseTest nonEmptyList "[a]"  -- 최소 1개 요소
  
  -- endBy
  putStrLn "\n7. endBy (구분자로 끝):"
  parseTest endByList "a,b,c,"
  
  -- sepEndBy
  putStrLn "\n8. sepEndBy (선택적 끝):"
  parseTest sepEndByList "a,b,c"
  parseTest sepEndByList "a,b,c,"
  
  -- count
  putStrLn "\n9. count (정확한 개수):"
  parseTest exactCountList "abc"
  parseTest exactCountList "ab"  -- 실패 (2개만 있음)
  
  -- between
  putStrLn "\n10. between (특정 문자로 감싸기):"
  parseTest betweenList "(a,b,c)"
