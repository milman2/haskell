{-# LANGUAGE OverloadedStrings #-}
module Step4.CustomErrorMessages where

import Text.Parsec
import Text.Parsec.String

-- 4️⃣ 에러 처리와 디버깅
-- <?>로 사용자 정의 에러 메시지

-- 기본 에러 메시지
basicParser :: Parser String
basicParser = do
  string "hello"
  return "matched"

-- 사용자 정의 에러 메시지
customErrorParser :: Parser String
customErrorParser = do
  try (string "hello") <?> "keyword 'hello'"
  return "matched"

-- 여러 에러 메시지
multipleErrorParser :: Parser String
multipleErrorParser = do
  choice [ try (string "hello") <?> "keyword 'hello'",
           try (string "hi")    <?> "keyword 'hi'",
           try (string "hey")   <?> "keyword 'hey'" ]
  return "matched"

-- 중첩된 에러 메시지
nestedErrorParser :: Parser String
nestedErrorParser = do
  try (char '(') <?> "opening parenthesis '('"
  content <- many (noneOf "()") <?> "content"
  try (char ')') <?> "closing parenthesis ')'"
  return content

-- 복잡한 에러 메시지
complexErrorParser :: Parser (String, Int)
complexErrorParser = do
  var <- try (many1 letter) <?> "variable name"
  spaces
  try (char '=') <?> "'='"
  spaces
  value <- try (many1 digit) <?> "number"
  return (var, read value)

-- JSON 스타일 에러 메시지
jsonErrorParser :: Parser (String, String)
jsonErrorParser = do
  try (char '"') <?> "opening quote"
  key <- many (noneOf "\"") <?> "key"
  try (char '"') <?> "closing quote"
  spaces
  try (char ':') <?> "colon"
  spaces
  try (char '"') <?> "opening quote"
  value <- many (noneOf "\"") <?> "value"
  try (char '"') <?> "closing quote"
  return (key, value)

-- 함수 호출 에러 메시지
functionCallErrorParser :: Parser (String, [String])
functionCallErrorParser = do
  funcName <- try (many1 letter) <?> "function name"
  try (char '(') <?> "opening parenthesis"
  spaces
  args <- sepBy (many1 letter <?> "argument") (try (char ',') <?> "comma ','")
  spaces
  try (char ')') <?> "closing parenthesis"
  return (funcName, args)

-- 배열 접근 에러 메시지
arrayAccessErrorParser :: Parser (String, Int)
arrayAccessErrorParser = do
  arrayName <- try (many1 letter) <?> "array name"
  try (char '[') <?> "opening bracket '['"
  spaces
  index <- try (many1 digit) <?> "array index"
  spaces
  try (char ']') <?> "closing bracket ']'"
  return (arrayName, read index)

-- 조건문 에러 메시지
ifStatementErrorParser :: Parser (String, String)
ifStatementErrorParser = do
  try (string "if") <?> "keyword 'if'"
  spaces
  condition <- try (many1 letter) <?> "condition"
  spaces
  try (char '{') <?> "opening brace '{'"
  spaces
  body <- many (noneOf "}") <?> "statement body"
  spaces
  try (char '}') <?> "closing brace '}'"
  return (condition, body)

-- 반복문 에러 메시지
whileStatementErrorParser :: Parser (String, String)
whileStatementErrorParser = do
  try (string "while") <?> "keyword 'while'"
  spaces
  condition <- try (many1 letter) <?> "condition"
  spaces
  try (char '{') <?> "opening brace '{'"
  spaces
  body <- many (noneOf "}") <?> "statement body"
  spaces
  try (char '}') <?> "closing brace '}'"
  return (condition, body)

-- 할당문 에러 메시지
assignmentErrorParser :: Parser (String, String)
assignmentErrorParser = do
  var <- try (many1 letter) <?> "variable name"
  spaces
  try (char '=') <?> "'='"
  spaces
  value <- many1 (letter <|> digit) <?> "value"
  return (var, value)

-- 출력문 에러 메시지
printStatementErrorParser :: Parser String
printStatementErrorParser = do
  try (string "print") <?> "keyword 'print'"
  spaces
  try (char '(') <?> "opening parenthesis"
  spaces
  value <- many1 (letter <|> digit) <?> "value to print"
  spaces
  try (char ')') <?> "closing parenthesis"
  return value

main :: IO ()
main = do
  putStrLn "=== 사용자 정의 에러 메시지 예제 ==="
  
  -- 기본 에러 메시지
  putStrLn "\n1. 기본 에러 메시지:"
  parseTest basicParser "hello"
  parseTest basicParser "hi"  -- 기본 에러 메시지
  
  -- 사용자 정의 에러 메시지
  putStrLn "\n2. 사용자 정의 에러 메시지:"
  parseTest customErrorParser "hello"
  parseTest customErrorParser "hi"  -- 사용자 정의 에러 메시지
  
  -- 여러 에러 메시지
  putStrLn "\n3. 여러 에러 메시지:"
  parseTest multipleErrorParser "hello"
  parseTest multipleErrorParser "hi"
  parseTest multipleErrorParser "hey"
  parseTest multipleErrorParser "ho"  -- 에러 메시지
  
  -- 중첩된 에러 메시지
  putStrLn "\n4. 중첩된 에러 메시지:"
  parseTest nestedErrorParser "(hello)"
  parseTest nestedErrorParser "(hello"  -- 에러 메시지
  parseTest nestedErrorParser "hello)"  -- 에러 메시지
  
  -- 복잡한 에러 메시지
  putStrLn "\n5. 복잡한 에러 메시지:"
  parseTest complexErrorParser "x = 42"
  parseTest complexErrorParser "x 42"  -- 에러 메시지
  parseTest complexErrorParser "= 42"  -- 에러 메시지
  
  -- JSON 스타일 에러 메시지
  putStrLn "\n6. JSON 스타일 에러 메시지:"
  parseTest jsonErrorParser "\"name\":\"john\""
  parseTest jsonErrorParser "name:\"john\""  -- 에러 메시지
  
  -- 함수 호출 에러 메시지
  putStrLn "\n7. 함수 호출 에러 메시지:"
  parseTest functionCallErrorParser "func(a,b,c)"
  parseTest functionCallErrorParser "func(a,b"  -- 에러 메시지
  
  -- 배열 접근 에러 메시지
  putStrLn "\n8. 배열 접근 에러 메시지:"
  parseTest arrayAccessErrorParser "arr[5]"
  parseTest arrayAccessErrorParser "arr[5"  -- 에러 메시지
  
  -- 조건문 에러 메시지
  putStrLn "\n9. 조건문 에러 메시지:"
  parseTest ifStatementErrorParser "if x { print x }"
  parseTest ifStatementErrorParser "if x { print x"  -- 에러 메시지
  
  -- 반복문 에러 메시지
  putStrLn "\n10. 반복문 에러 메시지:"
  parseTest whileStatementErrorParser "while x { print x }"
  parseTest whileStatementErrorParser "while x { print x"  -- 에러 메시지
  
  -- 할당문 에러 메시지
  putStrLn "\n11. 할당문 에러 메시지:"
  parseTest assignmentErrorParser "x = 42"
  parseTest assignmentErrorParser "x 42"  -- 에러 메시지
  
  -- 출력문 에러 메시지
  putStrLn "\n12. 출력문 에러 메시지:"
  parseTest printStatementErrorParser "print(x)"
  parseTest printStatementErrorParser "print(x"  -- 에러 메시지
