{-# LANGUAGE OverloadedStrings #-}
module Step3.ASTGeneration where

import Text.Parsec
import Text.Parsec.String

-- 3️⃣ 구조화된 데이터 파싱
-- AST 생성

-- 간단한 수식 AST
data Expr = Number Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show, Eq)

-- 수식 파서
expr :: Parser Expr
expr = do
  spaces
  result <- term
  spaces
  return result

term :: Parser Expr
term = do
  left <- factor
  rest <- many (try addOp <|> try subOp)
  return (foldl (\acc (op, right) -> op acc right) left rest)
  where
    addOp = do
      spaces
      char '+'
      spaces
      right <- factor
      return (Add, right)
    subOp = do
      spaces
      char '-'
      spaces
      right <- factor
      return (Sub, right)

factor :: Parser Expr
factor = do
  left <- primary
  rest <- many (try mulOp <|> try divOp)
  return (foldl (\acc (op, right) -> op acc right) left rest)
  where
    mulOp = do
      spaces
      char '*'
      spaces
      right <- primary
      return (Mul, right)
    divOp = do
      spaces
      char '/'
      spaces
      right <- primary
      return (Div, right)

primary :: Parser Expr
primary = do
  spaces
  choice [number, variable, parentheses]
  where
    number = do
      digits <- many1 digit
      return (Number (read digits))
    variable = do
      name <- many1 letter
      return (Var name)
    parentheses = do
      char '('
      result <- expr
      char ')'
      return result

-- 간단한 프로그래밍 언어 AST
data Statement = Assign String Expr
               | Print Expr
               | If Expr [Statement]
               | While Expr [Statement]
               deriving (Show, Eq)

-- 문장 파서
statement :: Parser Statement
statement = do
  spaces
  choice [try printStmt, try ifStmt, try whileStmt, try assignStmt]

assignStmt :: Parser Statement
assignStmt = do
  var <- many1 letter
  spaces
  char '='
  spaces
  expr <- expr
  return (Assign var expr)

printStmt :: Parser Statement
printStmt = do
  string "print"
  spaces
  expr <- expr
  return (Print expr)

ifStmt :: Parser Statement
ifStmt = do
  string "if"
  spaces
  condition <- expr
  spaces
  char '{'
  spaces
  stmts <- many statement
  spaces
  char '}'
  return (If condition stmts)

whileStmt :: Parser Statement
whileStmt = do
  string "while"
  spaces
  condition <- expr
  spaces
  char '{'
  spaces
  stmts <- many statement
  spaces
  char '}'
  return (While condition stmts)

-- JSON 스타일 AST
data JsonValue = JsonString String
               | JsonNumber Double
               | JsonBool Bool
               | JsonNull
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

-- JSON 파서
jsonValue :: Parser JsonValue
jsonValue = do
  spaces
  choice [jsonString, jsonNumber, jsonBool, jsonNull, jsonArray, jsonObject]

jsonString :: Parser JsonValue
jsonString = do
  char '"'
  content <- many (noneOf "\"")
  char '"'
  return (JsonString content)

jsonNumber :: Parser JsonValue
jsonNumber = do
  sign <- option 1 (char '-' >> return (-1))
  whole <- many1 digit
  decimal <- option "" (char '.' >> many1 digit)
  let number = read (whole ++ decimal)
  return (JsonNumber (fromIntegral sign * number))

jsonBool :: Parser JsonValue
jsonBool = do
  choice [string "true" >> return (JsonBool True),
          string "false" >> return (JsonBool False)]

jsonNull :: Parser JsonValue
jsonNull = do
  string "null"
  return JsonNull

jsonArray :: Parser JsonValue
jsonArray = do
  char '['
  spaces
  values <- sepBy jsonValue (char ',')
  spaces
  char ']'
  return (JsonArray values)

jsonObject :: Parser JsonValue
jsonObject = do
  char '{'
  spaces
  pairs <- sepBy jsonPair (char ',')
  spaces
  char '}'
  return (JsonObject pairs)

jsonPair :: Parser (String, JsonValue)
jsonPair = do
  spaces
  char '"'
  key <- many (noneOf "\"")
  char '"'
  spaces
  char ':'
  value <- jsonValue
  return (key, value)

-- AST 평가 함수 (수식)
eval :: Expr -> Int
eval (Number n) = n
eval (Var _) = 0  -- 데모용: 변수 값은 0으로 처리
eval (Add left right) = eval left + eval right
eval (Sub left right) = eval left - eval right
eval (Mul left right) = eval left * eval right
eval (Div left right) = eval left `div` eval right

main :: IO ()
main = do
  putStrLn "=== AST 생성 예제 ==="
  
  -- 수식 파싱
  putStrLn "\n1. 수식 파싱:"
  parseTest expr "1 + 2 * 3"
  parseTest expr "(1 + 2) * 3"
  parseTest expr "10 / 2 - 1"
  
  -- 수식 평가
  putStrLn "\n2. 수식 평가:"
  case parse expr "" "1 + 2 * 3" of
    Right ast -> putStrLn $ "AST: " ++ show ast ++ ", 결과: " ++ show (eval ast)
    Left err -> putStrLn $ "오류: " ++ show err
  
  -- 문장 파싱
  putStrLn "\n3. 문장 파싱:"
  parseTest statement "x = 5"
  parseTest statement "print x"
  parseTest statement "if x { print x }"
  
  -- JSON 파싱
  putStrLn "\n4. JSON 파싱:"
  parseTest jsonValue "\"hello\""
  parseTest jsonValue "123"
  parseTest jsonValue "true"
  parseTest jsonValue "null"
  parseTest jsonValue "[1, 2, 3]"
  parseTest jsonValue "{\"name\":\"john\",\"age\":25}"
