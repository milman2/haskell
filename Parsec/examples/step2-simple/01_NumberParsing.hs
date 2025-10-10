{-# LANGUAGE OverloadedStrings #-}
module Step2.NumberParsing where

import Text.Parsec
import Text.Parsec.String

-- 2️⃣ 간단한 파서 작성
-- 숫자, 문자, 문자열 파싱

-- 정수 파싱 (양수)
positiveInt :: Parser Int
positiveInt = do
  digits <- many1 digit
  return (read digits)

-- 정수 파싱 (양수, 음수)
signedInt :: Parser Int
signedInt = do
  sign <- option 1 (char '-' >> return (-1))
  digits <- many1 digit
  return (sign * read digits)

-- 부동소수점 파싱
floatNumber :: Parser Double
floatNumber = do
  sign <- option 1 (char '-' >> return (-1))
  whole <- many1 digit
  char '.'
  decimal <- many1 digit
  let number = read (whole ++ "." ++ decimal)
  return (fromIntegral sign * number)

-- 16진수 파싱
hexNumber :: Parser Int
hexNumber = do
  string "0x" <|> string "0X"
  hexDigits <- many1 hexDigit
  return (read ("0x" ++ hexDigits))

-- 자연수 파싱 (0 포함)
naturalNumber :: Parser Int
naturalNumber = do
  digits <- many1 digit
  return (read digits)

-- 양의 정수 파싱 (0 제외)
positiveNumber :: Parser Int
positiveNumber = do
  firstDigit <- oneOf "123456789"
  restDigits <- many digit
  return (read (firstDigit : restDigits))

main :: IO ()
main = do
  putStrLn "=== 숫자 파싱 예제 ==="
  
  -- 양수 정수
  putStrLn "\n1. 양수 정수 파싱:"
  parseTest positiveInt "123"
  parseTest positiveInt "0"
  parseTest positiveInt "-123"  -- 실패
  
  -- 부호 있는 정수
  putStrLn "\n2. 부호 있는 정수 파싱:"
  parseTest signedInt "123"
  parseTest signedInt "-123"
  parseTest signedInt "0"
  
  -- 부동소수점
  putStrLn "\n3. 부동소수점 파싱:"
  parseTest floatNumber "3.14"
  parseTest floatNumber "-2.5"
  parseTest floatNumber "0.0"
  
  -- 16진수
  putStrLn "\n4. 16진수 파싱:"
  parseTest hexNumber "0xFF"
  parseTest hexNumber "0x1A"
  parseTest hexNumber "0x0"
  
  -- 자연수
  putStrLn "\n5. 자연수 파싱:"
  parseTest naturalNumber "0"
  parseTest naturalNumber "123"
  
  -- 양의 정수
  putStrLn "\n6. 양의 정수 파싱:"
  parseTest positiveNumber "123"
  parseTest positiveNumber "0"  -- 실패
