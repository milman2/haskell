{-# LANGUAGE OverloadedStrings #-}
module Step1.ParseFunction where

import Text.Parsec
import Text.Parsec.String

-- 1️⃣ 기본 개념 익히기
-- parse 함수로 입력을 파싱

-- parse 함수의 타입:
-- parse :: Stream s Identity t => Parsec s () a -> SourceName -> s -> Either ParseError a

-- 파서 정의
simpleParser :: Parser String
simpleParser = many1 letter  -- 1개 이상의 알파벳 문자

numberParser :: Parser Int
numberParser = do
  digits <- many1 digit
  return (read digits)

-- parse 함수 사용 예제
main :: IO ()
main = do
  putStrLn "=== parse 함수 이해하기 ==="
  
  -- 성공적인 파싱
  putStrLn "\n1. 성공적인 파싱:"
  case parse simpleParser "test" "hello" of
    Right result -> putStrLn $ "성공: " ++ result
    Left error   -> putStrLn $ "실패: " ++ show error
  
  -- 실패한 파싱
  putStrLn "\n2. 실패한 파싱:"
  case parse simpleParser "test" "123" of
    Right result -> putStrLn $ "성공: " ++ result
    Left error   -> putStrLn $ "실패: " ++ show error
  
  -- 숫자 파싱
  putStrLn "\n3. 숫자 파싱:"
  case parse numberParser "test" "123" of
    Right result -> putStrLn $ "성공: " ++ show result
    Left error   -> putStrLn $ "실패: " ++ show error
  
  -- 빈 문자열 파싱
  putStrLn "\n4. 빈 문자열 파싱:"
  case parse simpleParser "test" "" of
    Right result -> putStrLn $ "성공: " ++ result
    Left error   -> putStrLn $ "실패: " ++ show error
  
  -- 부분 파싱 (전체 입력을 소비하지 않음)
  putStrLn "\n5. 부분 파싱:"
  case parse simpleParser "test" "hello123" of
    Right result -> putStrLn $ "성공: " ++ result
    Left error   -> putStrLn $ "실패: " ++ show error
  
  -- parseTest 함수 사용 (디버깅용)
  putStrLn "\n6. parseTest 함수 사용:"
  putStrLn "parseTest simpleParser \"hello\":"
  parseTest simpleParser "hello"
  
  putStrLn "\nparseTest simpleParser \"123\":"
  parseTest simpleParser "123"
