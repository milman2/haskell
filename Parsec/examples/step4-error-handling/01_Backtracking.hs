{-# LANGUAGE OverloadedStrings #-}
module Step4.Backtracking where

import Text.Parsec
import Text.Parsec.String

-- 4️⃣ 에러 처리와 디버깅
-- try로 백트래킹

-- try 없이 백트래킹 시도
withoutTry :: Parser String
withoutTry = do
  string "hello" <|> string "hi"
  return "matched"

-- try로 백트래킹 허용
withTry :: Parser String
withTry = do
  try (string "hello") <|> string "hi"
  return "matched"

-- 복잡한 백트래킹 예제
complexBacktracking :: Parser String
complexBacktracking = do
  choice [try (string "hello" >> string "world"),
          try (string "hi" >> string "there"),
          string "goodbye"]
  return "matched"

-- 중첩된 try
nestedTry :: Parser String
nestedTry = do
  try (do
    string "if"
    spaces
    try (string "true") <|> string "false"
    spaces
    string "then") <|> string "else"
  return "matched"

-- try와 choice 조합
tryWithChoice :: Parser String
tryWithChoice = do
  choice [try (string "function"),
          try (string "class"),
          try (string "interface"),
          string "variable"]
  return "matched"

-- try로 키워드 파싱
keywordWithTry :: String -> Parser String
keywordWithTry kw = do
  result <- try (string kw)
  notFollowedBy (letter <|> digit <|> char '_')
  return result

-- try로 연산자 파싱
operatorWithTry :: String -> Parser String
operatorWithTry op = do
  result <- try (string op)
  notFollowedBy (oneOf "+-*/=<>!")
  return result

-- try로 식별자 파싱
identifierWithTry :: Parser String
identifierWithTry = try $ do
  first <- letter
  rest <- many (letter <|> digit <|> char '_')
  return (first : rest)

-- try로 숫자 파싱
numberWithTry :: Parser Int
numberWithTry = try $ do
  digits <- many1 digit
  return (read digits)

-- try로 문자열 파싱
stringWithTry :: Parser String
stringWithTry = try $ do
  char '"'
  content <- many (noneOf "\"")
  char '"'
  return content

-- try로 괄호 파싱
parenthesesWithTry :: Parser String
parenthesesWithTry = try $ do
  char '('
  content <- many (noneOf "()")
  char ')'
  return content

-- try로 리스트 파싱
listWithTry :: Parser [String]
listWithTry = do
  char '['
  spaces
  items <- sepBy (try stringWithTry <|> try (many1 letter)) (char ',')
  spaces
  char ']'
  return items

main :: IO ()
main = do
  putStrLn "=== 백트래킹 예제 ==="
  
  -- try 없이 백트래킹
  putStrLn "\n1. try 없이 백트래킹:"
  parseTest withoutTry "hello"
  parseTest withoutTry "hi"
  parseTest withoutTry "hey"  -- 실패
  
  -- try로 백트래킹
  putStrLn "\n2. try로 백트래킹:"
  parseTest withTry "hello"
  parseTest withTry "hi"
  parseTest withTry "hey"  -- 실패
  
  -- 복잡한 백트래킹
  putStrLn "\n3. 복잡한 백트래킹:"
  parseTest complexBacktracking "helloworld"
  parseTest complexBacktracking "hithere"
  parseTest complexBacktracking "goodbye"
  parseTest complexBacktracking "hellothere"  -- 실패
  
  -- 중첩된 try
  putStrLn "\n4. 중첩된 try:"
  parseTest nestedTry "if true then"
  parseTest nestedTry "if false then"
  parseTest nestedTry "else"
  parseTest nestedTry "if maybe then"  -- 실패
  
  -- try와 choice 조합
  putStrLn "\n5. try와 choice 조합:"
  parseTest tryWithChoice "function"
  parseTest tryWithChoice "class"
  parseTest tryWithChoice "interface"
  parseTest tryWithChoice "variable"
  parseTest tryWithChoice "func"  -- 실패
  
  -- 키워드와 try
  putStrLn "\n6. 키워드와 try:"
  parseTest (keywordWithTry "if") "if"
  parseTest (keywordWithTry "if") "ifelse"  -- 실패
  
  -- 연산자와 try
  putStrLn "\n7. 연산자와 try:"
  parseTest (operatorWithTry "+") "+"
  parseTest (operatorWithTry "+") "++"  -- 실패
  
  -- 식별자와 try
  putStrLn "\n8. 식별자와 try:"
  parseTest identifierWithTry "variable"
  parseTest identifierWithTry "var123"
  
  -- 숫자와 try
  putStrLn "\n9. 숫자와 try:"
  parseTest numberWithTry "123"
  parseTest numberWithTry "0"
  
  -- 문자열과 try
  putStrLn "\n10. 문자열과 try:"
  parseTest stringWithTry "\"hello\""
  parseTest stringWithTry "\"world\""
  
  -- 괄호와 try
  putStrLn "\n11. 괄호와 try:"
  parseTest parenthesesWithTry "(hello)"
  parseTest parenthesesWithTry "(world)"
  
  -- 리스트와 try
  putStrLn "\n12. 리스트와 try:"
  parseTest listWithTry "[\"hello\",\"world\"]"
  parseTest listWithTry "[a,b,c]"
