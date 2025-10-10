{-# LANGUAGE OverloadedStrings #-}
module Step1.ParserType where

import Text.Parsec
import Text.Parsec.String

-- 1️⃣ 기본 개념 익히기
-- Parser a 타입의 의미

-- Parser a는 입력 문자열을 파싱하여 a 타입의 값을 반환하는 파서입니다.
-- 예: Parser Char - 문자를 파싱하는 파서
-- 예: Parser Int - 정수를 파싱하는 파서
-- 예: Parser String - 문자열을 파싱하는 파서

-- 기본적인 파서 예제들
charParser :: Parser Char
charParser = char 'a'  -- 'a' 문자만 파싱

stringParser :: Parser String
stringParser = string "hello"  -- "hello" 문자열만 파싱

digitParser :: Parser Char
digitParser = digit  -- 숫자 문자 하나 파싱

letterParser :: Parser Char
letterParser = letter  -- 알파벳 문자 하나 파싱

-- 파서 실행 예제
main :: IO ()
main = do
  putStrLn "=== Parser a 타입 이해하기 ==="
  
  -- char 파서 테스트
  putStrLn "\n1. char 'a' 파서:"
  print $ parse charParser "" "a"      -- Right 'a'
  print $ parse charParser "" "b"      -- Left (error)
  
  -- string 파서 테스트
  putStrLn "\n2. string \"hello\" 파서:"
  print $ parse stringParser "" "hello"    -- Right "hello"
  print $ parse stringParser "" "world"    -- Left (error)
  
  -- digit 파서 테스트
  putStrLn "\n3. digit 파서:"
  print $ parse digitParser "" "5"     -- Right '5'
  print $ parse digitParser "" "a"     -- Left (error)
  
  -- letter 파서 테스트
  putStrLn "\n4. letter 파서:"
  print $ parse letterParser "" "x"    -- Right 'x'
  print $ parse letterParser "" "5"    -- Left (error)
