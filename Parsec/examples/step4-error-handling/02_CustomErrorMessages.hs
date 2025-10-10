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
  string "hello" <?> "expected 'hello'"
  return "matched"

-- 여러 에러 메시지
multipleErrorParser :: Parser String
multipleErrorParser = do
  choice [string "hello" <?> "expected 'hello'",
          string "hi" <?> "expected 'hi'",
          string "hey" <?> "expected 'hey'"]
  return "matched"

-- 중첩된 에러 메시지
nestedErrorParser :: Parser String
nestedErrorParser = do
  char '(' <?> "expected opening parenthesis"
  content <- many (noneOf "()") <?> "expected content"
  char ')' <?> "expected closing parenthesis"
  return content

-- 복잡한 에러 메시지
complexErrorParser :: Parser (String, Int)
complexErrorParser = do
  var <- many1 letter <?> "expected variable name"
  spaces
  char '=' <?> "expected '='"
  spaces
  value <- many1 digit <?> "expected number"
  return (var, read value)

-- JSON 스타일 에러 메시지
jsonErrorParser :: Parser (String, String)
jsonErrorParser = do
  char '"' <?> "expected opening quote"
  key <- many (noneOf "\"") <?> "expected key"
  char '"' <?> "expected closing quote"
  spaces
  char ':' <?> "expected ':'"
  spaces
  char '"' <?> "expected opening quote"
  value <- many (noneOf "\"") <?> "expected value"
  char '"' <?> "expected closing quote"
  return (key, value)

-- 함수 호출 에러 메시지
functionCallErrorParser :: Parser (String, [String])
functionCallErrorParser = do
  funcName <- many1 letter <?> "expected function name"
  char '(' <?> "expected opening parenthesis"
  spaces
  args <- sepBy (many1 letter <?> "expected argument") (char ',' <?> "expected comma")
  spaces
  char ')' <?> "expected closing parenthesis"
  return (funcName, args)

-- 배열 접근 에러 메시지
arrayAccessErrorParser :: Parser (String, Int)
arrayAccessErrorParser = do
  arrayName <- many1 letter <?> "expected array name"
  char '[' <?> "expected opening bracket"
  spaces
  index <- many1 digit <?> "expected array index"
  spaces
  char ']' <?> "expected closing bracket"
  return (arrayName, read index)

-- 조건문 에러 메시지
ifStatementErrorParser :: Parser (String, String)
ifStatementErrorParser = do
  string "if" <?> "expected 'if'"
  spaces
  condition <- many1 letter <?> "expected condition"
  spaces
  char '{' <?> "expected opening brace"
  spaces
  body <- many (noneOf "}") <?> "expected statement body"
  spaces
  char '}' <?> "expected closing brace"
  return (condition, body)

-- 반복문 에러 메시지
whileStatementErrorParser :: Parser (String, String)
whileStatementErrorParser = do
  string "while" <?> "expected 'while'"
  spaces
  condition <- many1 letter <?> "expected condition"
  spaces
  char '{' <?> "expected opening brace"
  spaces
  body <- many (noneOf "}") <?> "expected statement body"
  spaces
  char '}' <?> "expected closing brace"
  return (condition, body)

-- 할당문 에러 메시지
assignmentErrorParser :: Parser (String, String)
assignmentErrorParser = do
  var <- many1 letter <?> "expected variable name"
  spaces
  char '=' <?> "expected '='"
  spaces
  value <- many1 (letter <|> digit) <?> "expected value"
  return (var, value)

-- 출력문 에러 메시지
printStatementErrorParser :: Parser String
printStatementErrorParser = do
  string "print" <?> "expected 'print'"
  spaces
  char '(' <?> "expected opening parenthesis"
  spaces
  value <- many1 (letter <|> digit) <?> "expected value to print"
  spaces
  char ')' <?> "expected closing parenthesis"
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
