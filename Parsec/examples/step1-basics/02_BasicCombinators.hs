{-# LANGUAGE OverloadedStrings #-}
module Step1.BasicCombinators where

import Text.Parsec
import Text.Parsec.String

-- 1️⃣ 기본 개념 익히기
-- char, string, many, choice, try 등 기본 컴비네이터

-- many: 0개 이상 반복
manyLetters :: Parser String
manyLetters = many letter  -- 0개 이상의 알파벳 문자

manyDigits :: Parser String
manyDigits = many digit    -- 0개 이상의 숫자

-- many1: 1개 이상 반복
many1Letters :: Parser String
many1Letters = many1 letter  -- 1개 이상의 알파벳 문자

-- choice (<|>): 여러 파서 중 하나 선택
letterOrDigit :: Parser Char
letterOrDigit = letter <|> digit  -- 알파벳 또는 숫자

-- try: 백트래킹 허용
tryExample :: Parser String
tryExample = try (string "hello") <|> string "hi"  -- "hello" 시도 후 실패하면 "hi" 시도

-- (<$>): Functor의 fmap, 파싱 결과를 함수로 변환
upperCase :: Parser Char
upperCase = toUpper <$> letter  -- 알파벳을 대문자로 변환
  where toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- (<*>): Applicative의 apply, 두 파서의 결과를 함수에 적용
pairParser :: Parser (Char, Char)
pairParser = (,) <$> letter <*> digit  -- (알파벳, 숫자) 쌍

-- (<*): 첫 번째 파서 결과만 반환, 두 번째는 소비만
letterThenDigit :: Parser Char
letterThenDigit = letter <* digit  -- 알파벳만 반환, 숫자는 소비만

-- (*>): 두 번째 파서 결과만 반환, 첫 번째는 소비만
digitAfterLetter :: Parser Char
digitAfterLetter = letter *> digit  -- 숫자만 반환, 알파벳은 소비만

main :: IO ()
main = do
  putStrLn "=== 기본 컴비네이터 이해하기 ==="
  
  -- many 테스트
  putStrLn "\n1. many letter 파서:"
  print $ parse manyLetters "" "abc"     -- Right "abc"
  print $ parse manyLetters "" "123"     -- Right "" (빈 문자열)
  print $ parse manyLetters "" ""        -- Right "" (빈 문자열)
  
  -- many1 테스트
  putStrLn "\n2. many1 letter 파서:"
  print $ parse many1Letters "" "abc"    -- Right "abc"
  print $ parse many1Letters "" "123"    -- Left (error)
  print $ parse many1Letters "" ""       -- Left (error)
  
  -- choice 테스트
  putStrLn "\n3. letter <|> digit 파서:"
  print $ parse letterOrDigit "" "a"     -- Right 'a'
  print $ parse letterOrDigit "" "5"     -- Right '5'
  print $ parse letterOrDigit "" "!"     -- Left (error)
  
  -- try 테스트
  putStrLn "\n4. try (string \"hello\") <|> string \"hi\" 파서:"
  print $ parse tryExample "" "hello"    -- Right "hello"
  print $ parse tryExample "" "hi"       -- Right "hi"
  print $ parse tryExample "" "hey"      -- Left (error)
  
  -- Functor 테스트
  putStrLn "\n5. toUpper <$> letter 파서:"
  print $ parse upperCase "" "a"         -- Right 'A'
  print $ parse upperCase "" "z"         -- Right 'Z'
  
  -- Applicative 테스트
  putStrLn "\n6. (,) <$> letter <*> digit 파서:"
  print $ parse pairParser "" "a5"       -- Right ('a','5')
  print $ parse pairParser "" "5a"       -- Left (error)
  
  -- <* 테스트
  putStrLn "\n7. letter <* digit 파서:"
  print $ parse letterThenDigit "" "a5"  -- Right 'a'
  
  -- *> 테스트
  putStrLn "\n8. letter *> digit 파서:"
  print $ parse digitAfterLetter "" "a5" -- Right '5'
