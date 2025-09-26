{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HaskellPDSL where

-- Haskell과 PDSL을 결합한 예제
-- 이 모듈은 Haskell에서 PDSL을 파싱하고 실행하는 예제입니다

import Data.Map (Map, fromList, lookup, insert)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.List (intercalate)
import Control.Monad (when)

-- 1. PDSL 문법 정의 (Haskell 데이터 타입으로)
data PDSLExpr
    = Number Double
    | String Text
    | Variable Text
    | Add PDSLExpr PDSLExpr
    | Subtract PDSLExpr PDSLExpr
    | Multiply PDSLExpr PDSLExpr
    | Divide PDSLExpr PDSLExpr
    | Equal PDSLExpr PDSLExpr
    | LessThan PDSLExpr PDSLExpr
    | GreaterThan PDSLExpr PDSLExpr
    | And PDSLExpr PDSLExpr
    | Or PDSLExpr PDSLExpr
    | Not PDSLExpr
    | If PDSLExpr PDSLExpr PDSLExpr
    | Function Text [Text] PDSLExpr
    | Call Text [PDSLExpr]
    | List [PDSLExpr]
    | Map [(Text, PDSLExpr)]
    deriving (Show, Eq)

-- 2. PDSL 환경 (변수와 함수 저장)
data PDSLEnv = PDSLEnv
    { variables :: Map Text PDSLValue
    , functions :: Map Text PDSLFunction
    } deriving (Show)

data PDSLValue
    = VNumber Double
    | VString Text
    | VBoolean Bool
    | VList [PDSLValue]
    | VMap (Map Text PDSLValue)
    | VFunction PDSLFunction
    deriving (Show, Eq)

data PDSLFunction = PDSLFunction
    { params :: [Text]
    , body :: PDSLExpr
    } deriving (Show, Eq)

-- 3. PDSL 파서 (간단한 파싱 함수들)
parsePDSL :: Text -> Either Text PDSLExpr
parsePDSL input = case parseExpr (words (unpack input)) of
    Left err -> Left (pack err)
    Right (expr, []) -> Right expr
    Right (_, remaining) -> Left ("Unexpected tokens: " `mappend` pack (unwords remaining))

parseExpr :: [String] -> Either String (PDSLExpr, [String])
parseExpr tokens = parseOr tokens

parseOr :: [String] -> Either String (PDSLExpr, [String])
parseOr tokens = do
    (left, tokens') <- parseAnd tokens
    case tokens' of
        ("||" : rest) -> do
            (right, tokens'') <- parseOr rest
            return (Or left right, tokens'')
        _ -> return (left, tokens')

parseAnd :: [String] -> Either String (PDSLExpr, [String])
parseAnd tokens = do
    (left, tokens') <- parseComparison tokens
    case tokens' of
        ("&&" : rest) -> do
            (right, tokens'') <- parseAnd rest
            return (And left right, tokens'')
        _ -> return (left, tokens')

parseComparison :: [String] -> Either String (PDSLExpr, [String])
parseComparison tokens = do
    (left, tokens') <- parseArithmetic tokens
    case tokens' of
        ("==" : rest) -> do
            (right, tokens'') <- parseArithmetic rest
            return (Equal left right, tokens'')
        ("<" : rest) -> do
            (right, tokens'') <- parseArithmetic rest
            return (LessThan left right, tokens'')
        (">" : rest) -> do
            (right, tokens'') <- parseArithmetic rest
            return (GreaterThan left right, tokens'')
        _ -> return (left, tokens')

parseArithmetic :: [String] -> Either String (PDSLExpr, [String])
parseArithmetic tokens = do
    (left, tokens') <- parseTerm tokens
    case tokens' of
        ("+" : rest) -> do
            (right, tokens'') <- parseArithmetic rest
            return (Add left right, tokens'')
        ("-" : rest) -> do
            (right, tokens'') <- parseArithmetic rest
            return (Subtract left right, tokens'')
        _ -> return (left, tokens')

parseTerm :: [String] -> Either String (PDSLExpr, [String])
parseTerm tokens = do
    (left, tokens') <- parseFactor tokens
    case tokens' of
        ("*" : rest) -> do
            (right, tokens'') <- parseTerm rest
            return (Multiply left right, tokens'')
        ("/" : rest) -> do
            (right, tokens'') <- parseTerm rest
            return (Divide left right, tokens'')
        _ -> return (left, tokens')

parseFactor :: [String] -> Either String (PDSLExpr, [String])
parseFactor tokens = case tokens of
    [] -> Left "Unexpected end of input"
    ("(" : rest) -> do
        (expr, tokens') <- parseExpr rest
        case tokens' of
            (")" : tokens'') -> return (expr, tokens'')
            _ -> Left "Missing closing parenthesis"
    ("!" : rest) -> do
        (expr, tokens') <- parseFactor rest
        return (Not expr, tokens')
    ("if" : rest) -> do
        (cond, tokens') <- parseExpr rest
        case tokens' of
            ("then" : tokens'') -> do
                (thenExpr, tokens''') <- parseExpr tokens''
                case tokens''' of
                    ("else" : tokens'''') -> do
                        (elseExpr, tokens''''') <- parseExpr tokens''''
                        return (If cond thenExpr elseExpr, tokens''''')
                    _ -> Left "Missing 'else' in if expression"
            _ -> Left "Missing 'then' in if expression"
    (token : rest) -> case token of
        x | all isDigit x || (head x == '-' && all isDigit (tail x)) -> 
            return (Number (read x), rest)
        x | head x == '"' && last x == '"' -> 
            return (String (pack (init (tail x))), rest)
        x -> return (Variable (pack x), rest)

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- 4. PDSL 실행기
evalPDSL :: PDSLEnv -> PDSLExpr -> Either Text PDSLValue
evalPDSL env (Number n) = Right (VNumber n)
evalPDSL env (String s) = Right (VString s)
evalPDSL env (Variable name) = case Map.lookup name (variables env) of
    Just value -> Right value
    Nothing -> Left ("Variable not found: " `mappend` name)
evalPDSL env (Add left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    case (l, r) of
        (VNumber a, VNumber b) -> Right (VNumber (a + b))
        (VString a, VString b) -> Right (VString (a `mappend` b))
        _ -> Left "Type error in addition"
evalPDSL env (Subtract left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    case (l, r) of
        (VNumber a, VNumber b) -> Right (VNumber (a - b))
        _ -> Left "Type error in subtraction"
evalPDSL env (Multiply left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    case (l, r) of
        (VNumber a, VNumber b) -> Right (VNumber (a * b))
        _ -> Left "Type error in multiplication"
evalPDSL env (Divide left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    case (l, r) of
        (VNumber a, VNumber b) -> if b == 0 then Left "Division by zero" else Right (VNumber (a / b))
        _ -> Left "Type error in division"
evalPDSL env (Equal left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    Right (VBoolean (l == r))
evalPDSL env (LessThan left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    case (l, r) of
        (VNumber a, VNumber b) -> Right (VBoolean (a < b))
        _ -> Left "Type error in comparison"
evalPDSL env (GreaterThan left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    case (l, r) of
        (VNumber a, VNumber b) -> Right (VBoolean (a > b))
        _ -> Left "Type error in comparison"
evalPDSL env (And left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    case (l, r) of
        (VBoolean a, VBoolean b) -> Right (VBoolean (a && b))
        _ -> Left "Type error in logical AND"
evalPDSL env (Or left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    case (l, r) of
        (VBoolean a, VBoolean b) -> Right (VBoolean (a || b))
        _ -> Left "Type error in logical OR"
evalPDSL env (Not expr) = do
    val <- evalPDSL env expr
    case val of
        VBoolean b -> Right (VBoolean (not b))
        _ -> Left "Type error in logical NOT"
evalPDSL env (If cond thenExpr elseExpr) = do
    condVal <- evalPDSL env cond
    case condVal of
        VBoolean True -> evalPDSL env thenExpr
        VBoolean False -> evalPDSL env elseExpr
        _ -> Left "Type error in if condition"
evalPDSL env (List exprs) = do
    vals <- mapM (evalPDSL env) exprs
    Right (VList vals)
evalPDSL env (Map pairs) = do
    vals <- mapM (\(k, v) -> do
        val <- evalPDSL env v
        return (k, val)) pairs
    Right (VMap (fromList vals))
evalPDSL env (Function name params body) = do
    let newEnv = env { functions = Map.insert name (PDSLFunction params body) (functions env) }
    Right (VFunction (PDSLFunction params body))
evalPDSL env (Call name args) = case Map.lookup name (functions env) of
    Just (PDSLFunction params body) -> do
        argVals <- mapM (evalPDSL env) args
        let newVars = Map.fromList (zip params argVals)
        let newEnv = env { variables = Map.union newVars (variables env) }
        evalPDSL newEnv body
    Nothing -> Left ("Function not found: " `mappend` name)

-- 5. PDSL 값 출력
showPDSLValue :: PDSLValue -> Text
showPDSLValue (VNumber n) = pack (show n)
showPDSLValue (VString s) = "\"" `mappend` s `mappend` "\""
showPDSLValue (VBoolean b) = if b then "true" else "false"
showPDSLValue (VList vals) = "[" `mappend` pack (intercalate ", " (map (unpack . showPDSLValue) vals)) `mappend` "]"
showPDSLValue (VMap m) = "{" `mappend` pack (intercalate ", " (map (\(k, v) -> unpack k `mappend` ": " `mappend` unpack (showPDSLValue v)) (Map.toList m))) `mappend` "}"
showPDSLValue (VFunction _) = "<function>"

-- 6. PDSL 인터프리터
runPDSL :: Text -> IO ()
runPDSL input = do
    case parsePDSL input of
        Left err -> putStrLn $ "Parse error: " ++ unpack err
        Right expr -> do
            let env = PDSLEnv Map.empty Map.empty
            case evalPDSL env expr of
                Left err -> putStrLn $ "Runtime error: " ++ unpack err
                Right val -> putStrLn $ "Result: " ++ unpack (showPDSLValue val)

-- 7. 사용 예시
example1 :: PDSLExpr
example1 = Add (Number 2) (Multiply (Number 3) (Number 4))

example2 :: PDSLExpr
example2 = If (GreaterThan (Variable "x") (Number 10))
              (Add (Variable "x") (Number 1))
              (Subtract (Variable "x") (Number 1))

example3 :: PDSLExpr
example3 = And (Equal (Variable "age") (Number 18))
               (Equal (Variable "name") (String "John"))

-- 8. 테스트 함수
testPDSL :: IO ()
testPDSL = do
    putStrLn "=== Haskell PDSL Examples ==="
    putStrLn ""
    
    putStrLn "Example 1: 2 + 3 * 4"
    runPDSL "2 + 3 * 4"
    putStrLn ""
    
    putStrLn "Example 2: 10 > 5 && 3 < 7"
    runPDSL "10 > 5 && 3 < 7"
    putStrLn ""
    
    putStrLn "Example 3: if 5 > 3 then 10 else 20"
    runPDSL "if 5 > 3 then 10 else 20"
    putStrLn ""
    
    putStrLn "Example 4: !(5 > 10)"
    runPDSL "!(5 > 10)"
    putStrLn ""
    
    putStrLn "Example 5: (2 + 3) * (4 - 1)"
    runPDSL "(2 + 3) * (4 - 1)"
    putStrLn ""
