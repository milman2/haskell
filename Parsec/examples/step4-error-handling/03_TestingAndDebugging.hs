{-# LANGUAGE OverloadedStrings #-}
module Step4.TestingAndDebugging where

import Text.Parsec
import Text.Parsec.String
import System.IO

-- 4️⃣ 에러 처리와 디버깅
-- parseTest, parseFromFile로 테스트

-- 간단한 파서
simpleParser :: Parser String
simpleParser = do
  spaces
  word <- many1 letter
  spaces
  return word

-- 복잡한 파서
complexParser :: Parser (String, Int)
complexParser = do
  spaces
  var <- many1 letter
  spaces
  char '='
  spaces
  value <- many1 digit
  spaces
  return (var, read value)

-- JSON 스타일 파서
jsonParser :: Parser (String, String)
jsonParser = do
  spaces
  char '"'
  key <- many (noneOf "\"")
  char '"'
  spaces
  char ':'
  spaces
  char '"'
  value <- many (noneOf "\"")
  char '"'
  spaces
  return (key, value)

-- 함수 호출 파서
functionCallParser :: Parser (String, [String])
functionCallParser = do
  spaces
  funcName <- many1 letter
  spaces
  char '('
  spaces
  args <- sepBy (many1 letter) (char ',')
  spaces
  char ')'
  spaces
  return (funcName, args)

-- parseTest 함수 사용 예제
testWithParseTest :: IO ()
testWithParseTest = do
  putStrLn "=== parseTest 함수 사용 예제 ==="
  
  putStrLn "\n1. 간단한 파서 테스트:"
  parseTest simpleParser "hello"
  parseTest simpleParser "  world  "
  parseTest simpleParser "123"  -- 실패
  
  putStrLn "\n2. 복잡한 파서 테스트:"
  parseTest complexParser "x = 42"
  parseTest complexParser "  y  =  123  "
  parseTest complexParser "x 42"  -- 실패
  
  putStrLn "\n3. JSON 파서 테스트:"
  parseTest jsonParser "\"name\":\"john\""
  parseTest jsonParser "  \"age\"  :  \"25\"  "
  parseTest jsonParser "name:john"  -- 실패
  
  putStrLn "\n4. 함수 호출 파서 테스트:"
  parseTest functionCallParser "func(a,b,c)"
  parseTest functionCallParser "  print  (  hello  )  "
  parseTest functionCallParser "func(a,b"  -- 실패

-- parseFromFile 함수 사용 예제
testWithParseFromFile :: IO ()
testWithParseFromFile = do
  putStrLn "\n=== parseFromFile 함수 사용 예제 ==="
  
  -- 테스트 파일 생성
  writeFile "test_input.txt" "hello world"
  writeFile "test_assignment.txt" "x = 42\ny = 123"
  writeFile "test_json.txt" "\"name\":\"john\"\n\"age\":\"25\""
  writeFile "test_function.txt" "func(a,b,c)\nprint(hello)"
  
  putStrLn "\n1. 간단한 파서로 파일 파싱:"
  result1 <- parseFromFile simpleParser "test_input.txt"
  case result1 of
    Right value -> putStrLn $ "성공: " ++ value
    Left error -> putStrLn $ "실패: " ++ show error
  
  putStrLn "\n2. 복잡한 파서로 파일 파싱:"
  result2 <- parseFromFile complexParser "test_assignment.txt"
  case result2 of
    Right value -> putStrLn $ "성공: " ++ show value
    Left error -> putStrLn $ "실패: " ++ show error
  
  putStrLn "\n3. JSON 파서로 파일 파싱:"
  result3 <- parseFromFile jsonParser "test_json.txt"
  case result3 of
    Right value -> putStrLn $ "성공: " ++ show value
    Left error -> putStrLn $ "실패: " ++ show error
  
  putStrLn "\n4. 함수 호출 파서로 파일 파싱:"
  result4 <- parseFromFile functionCallParser "test_function.txt"
  case result4 of
    Right value -> putStrLn $ "성공: " ++ show value
    Left error -> putStrLn $ "실패: " ++ show error
  
  -- 테스트 파일 삭제
  mapM_ removeFile ["test_input.txt", "test_assignment.txt", "test_json.txt", "test_function.txt"]

-- 에러 메시지 분석 예제
analyzeErrors :: IO ()
analyzeErrors = do
  putStrLn "\n=== 에러 메시지 분석 예제 ==="
  
  putStrLn "\n1. 파싱 성공:"
  case parse simpleParser "test" "hello" of
    Right result -> putStrLn $ "결과: " ++ result
    Left error -> putStrLn $ "에러: " ++ show error
  
  putStrLn "\n2. 파싱 실패:"
  case parse simpleParser "test" "123" of
    Right result -> putStrLn $ "결과: " ++ result
    Left error -> putStrLn $ "에러: " ++ show error
  
  putStrLn "\n3. 부분 파싱:"
  case parse simpleParser "test" "hello123" of
    Right result -> putStrLn $ "결과: " ++ result
    Left error -> putStrLn $ "에러: " ++ show error
  
  putStrLn "\n4. 빈 입력:"
  case parse simpleParser "test" "" of
    Right result -> putStrLn $ "결과: " ++ result
    Left error -> putStrLn $ "에러: " ++ show error

-- 디버깅을 위한 파서 테스트
debugParser :: IO ()
debugParser = do
  putStrLn "\n=== 디버깅을 위한 파서 테스트 ==="
  
  putStrLn "\n1. 단계별 파서 테스트:"
  putStrLn "문자 파싱:"
  parseTest (many1 letter) "hello"
  parseTest (many1 letter) "123"
  
  putStrLn "\n숫자 파싱:"
  parseTest (many1 digit) "123"
  parseTest (many1 digit) "hello"
  
  putStrLn "\n공백 파싱:"
  parseTest spaces "   "
  parseTest spaces "hello"
  
  putStrLn "\n2. 조합 파서 테스트:"
  putStrLn "문자 + 공백:"
  parseTest (many1 letter <* spaces) "hello   "
  parseTest (many1 letter <* spaces) "hello"
  
  putStrLn "\n공백 + 문자:"
  parseTest (spaces *> many1 letter) "   hello"
  parseTest (spaces *> many1 letter) "hello"
  
  putStrLn "\n3. 복잡한 조합 테스트:"
  putStrLn "할당문 파싱:"
  parseTest complexParser "x = 42"
  parseTest complexParser "x=42"
  parseTest complexParser "x 42"
  parseTest complexParser "= 42"

-- 성능 테스트
performanceTest :: IO ()
performanceTest = do
  putStrLn "\n=== 성능 테스트 ==="
  
  -- 큰 입력 생성
  let bigInput = concat (replicate 1000 "hello ")
  
  putStrLn "\n1. 큰 입력 파싱:"
  putStrLn $ "입력 크기: " ++ show (length bigInput) ++ " 문자"
  
  -- 시간 측정 (간단한 방법)
  start <- getCPUTime
  let result = parse simpleParser "performance_test" bigInput
  end <- getCPUTime
  let time = (end - start) `div` 1000000000  -- 나노초를 밀리초로 변환
  
  case result of
    Right value -> putStrLn $ "성공: " ++ show (length value) ++ " 문자, 시간: " ++ show time ++ "ms"
    Left error -> putStrLn $ "실패: " ++ show error

-- getCPUTime 함수 (간단한 구현)
getCPUTime :: IO Integer
getCPUTime = return 0  -- 실제로는 System.CPUTime을 사용해야 함

main :: IO ()
main = do
  testWithParseTest
  testWithParseFromFile
  analyzeErrors
  debugParser
  performanceTest
