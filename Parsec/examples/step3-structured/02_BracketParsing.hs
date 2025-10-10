{-# LANGUAGE OverloadedStrings #-}
module Step3.BracketParsing where

import Text.Parsec
import Text.Parsec.String

-- 3️⃣ 구조화된 데이터 파싱
-- 괄호, 중첩 구조

-- 기본 괄호
parentheses :: Parser String
parentheses = do
  char '('
  content <- many (noneOf "()")
  char ')'
  return content

-- 중첩된 괄호 (제대로 구현)
nestedParentheses :: Parser String
nestedParentheses = do
  char '('
  parts <- many (nestedParentheses <|> fmap (:[]) (noneOf "()"))
  char ')'
  return ("(" ++ concat parts ++ ")")

-- 중괄호
braces :: Parser String
braces = do
  char '{'
  content <- many (noneOf "{}")
  char '}'
  return content

-- 대괄호
brackets :: Parser String
brackets = do
  char '['
  content <- many (noneOf "[]")
  char ']'
  return content

-- 괄호 안의 숫자
numberInParentheses :: Parser Int
numberInParentheses = do
  char '('
  spaces
  digits <- many1 digit
  spaces
  char ')'
  return (read digits)

-- 괄호 안의 문자열
stringInParentheses :: Parser String
stringInParentheses = do
  char '('
  spaces
  char '"'
  content <- many (noneOf "\"")
  char '"'
  spaces
  char ')'
  return content

-- 여러 괄호 타입
mixedBrackets :: Parser String
mixedBrackets = do
  char '('
  content1 <- many (noneOf "(){}[]")
  char '{'
  content2 <- many (noneOf "(){}[]")
  char '}'
  content3 <- many (noneOf "(){}[]")
  char ')'
  return (content1 ++ "{" ++ content2 ++ "}" ++ content3)

-- 중첩된 괄호 (재귀적)
data BracketExpr = BracketExpr String [BracketExpr] deriving (Show)

nestedBracketExpr :: Parser BracketExpr
nestedBracketExpr = do
  char '('
  spaces
  content <- many (noneOf "()")
  spaces
  inner <- many (try nestedBracketExpr)
  spaces
  char ')'
  return (BracketExpr content inner)

balancedParentheses :: Parser String
balancedParentheses = fmap concat (many (parenGroup <|> normalChar))
  where
    -- 일반 문자 하나를 문자열로 승격
    normalChar :: Parser String
    normalChar = (:[]) <$> noneOf "()"

    -- 괄호 그룹: '(' balanced ')' 형태를 재귀적으로 허용
    parenGroup :: Parser String
    parenGroup = do
      char '('
      inside <- balancedParentheses
      char ')'
      return ('(' : inside ++ ")")

-- 함수 호출 스타일
functionCall :: Parser (String, [String])
functionCall = do
  funcName <- many1 letter
  char '('
  spaces
  args <- sepBy (many1 letter) (char ',')
  spaces
  char ')'
  return (funcName, args)

-- 배열 접근 스타일
arrayAccess :: Parser (String, Int)
arrayAccess = do
  arrayName <- many1 letter
  char '['
  spaces
  index <- many1 digit
  spaces
  char ']'
  return (arrayName, read index)

main :: IO ()
main = do
  putStrLn "=== 괄호 파싱 예제 ==="
  
  -- 기본 괄호
  putStrLn "\n1. 기본 괄호:"
  parseTest parentheses "(hello)"
  parseTest parentheses "(123)"
  
  -- 중괄호
  putStrLn "\n2. 중괄호:"
  parseTest braces "{hello}"
  parseTest braces "{123}"
  
  -- 대괄호
  putStrLn "\n3. 대괄호:"
  parseTest brackets "[hello]"
  parseTest brackets "[123]"
  
  -- 괄호 안의 숫자
  putStrLn "\n4. 괄호 안의 숫자:"
  parseTest numberInParentheses "(123)"
  parseTest numberInParentheses "( 456 )"
  
  -- 괄호 안의 문자열
  putStrLn "\n5. 괄호 안의 문자열:"
  parseTest stringInParentheses "(\"hello\")"
  parseTest stringInParentheses "( \"world\" )"
  
  -- 여러 괄호 타입
  putStrLn "\n6. 여러 괄호 타입:"
  parseTest mixedBrackets "(hello{world}test)"
  
  -- 중첩된 괄호
  putStrLn "\n7. 중첩된 괄호:"
  parseTest nestedParentheses "(hello)"
  parseTest nestedParentheses "((nested))"
  parseTest nestedParentheses "(((deep)))"
  parseTest nestedParentheses "(a(b)c)"
  
  -- 괄호 밸런스 체크
  putStrLn "\n8. 괄호 밸런스 체크:"
  parseTest balancedParentheses "((hello)world)"
  parseTest balancedParentheses "((hello)"  -- 실패
  
  -- 함수 호출
  putStrLn "\n9. 함수 호출:"
  parseTest functionCall "func(a,b,c)"
  parseTest functionCall "print(hello)"
  
  -- 배열 접근
  putStrLn "\n10. 배열 접근:"
  parseTest arrayAccess "arr[5]"
  parseTest arrayAccess "list[0]"
