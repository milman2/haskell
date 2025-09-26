{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ConfigPDSL where

-- Haskell로 구현한 설정 파일 PDSL
-- 이 모듈은 설정 파일을 파싱하고 검증하는 PDSL을 구현합니다

import Data.Map (Map, fromList, lookup, insert)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack, splitOn)
import Data.List (intercalate)
import Control.Monad (when, unless)

-- 1. 설정 PDSL 문법 정의
data ConfigExpr
    = ConfigString Text
    | ConfigNumber Double
    | ConfigBoolean Bool
    | ConfigList [ConfigExpr]
    | ConfigSection Text [(Text, ConfigExpr)]
    | ConfigInclude Text
    | ConfigCondition ConfigExpr ConfigExpr ConfigExpr
    deriving (Show, Eq)

-- 2. 설정 환경
data ConfigEnv = ConfigEnv
    { configValues :: Map Text ConfigValue
    , configSections :: Map Text (Map Text ConfigValue)
    , configIncludes :: [Text]
    } deriving (Show)

data ConfigValue
    = CString Text
    | CNumber Double
    | CBoolean Bool
    | CList [ConfigValue]
    | CSection (Map Text ConfigValue)
    deriving (Show, Eq)

-- 3. 설정 PDSL 파서
parseConfig :: Text -> Either Text [ConfigExpr]
parseConfig input = parseConfigLines (lines (unpack input)) []

parseConfigLines :: [String] -> [ConfigExpr] -> Either Text [ConfigExpr]
parseConfigLines [] acc = Right (reverse acc)
parseConfigLines (line : rest) acc = do
    let trimmed = trim line
    if null trimmed || head trimmed == '#'
        then parseConfigLines rest acc
        else do
            expr <- parseConfigLine trimmed
            parseConfigLines rest (expr : acc)

parseConfigLine :: String -> Either Text ConfigExpr
parseConfigLine line = case splitOn '=' line of
    [key, value] -> do
        let key' = pack (trim key)
        let value' = trim value
        expr <- parseConfigValue value'
        return (ConfigSection key' [(key', expr)])
    _ -> Left ("Invalid config line: " `mappend` pack line)

parseConfigValue :: String -> Either Text ConfigExpr
parseConfigValue value = case value of
    x | head x == '"' && last x == '"' -> 
        Right (ConfigString (pack (init (tail x))))
    x | x == "true" -> Right (ConfigBoolean True)
    x | x == "false" -> Right (ConfigBoolean False)
    x | all isDigit x || (head x == '-' && all isDigit (tail x)) -> 
        Right (ConfigNumber (read x))
    x | head x == '[' && last x == ']' -> do
        let content = init (tail x)
        let items = map trim (splitOn ',' content)
        exprs <- mapM parseConfigValue items
        return (ConfigList exprs)
    x | head x == '{' && last x == '}' -> do
        let content = init (tail x)
        let pairs = map (splitOn ':') (splitOn ',' content)
        configPairs <- mapM (\(k : v : _) -> do
            let key = pack (trim k)
            expr <- parseConfigValue (trim v)
            return (key, expr)) pairs
        return (ConfigSection "anonymous" configPairs)
    x -> Right (ConfigString (pack x))

-- 4. 설정 PDSL 실행기
evalConfig :: ConfigEnv -> ConfigExpr -> Either Text ConfigValue
evalConfig env (ConfigString s) = Right (CString s)
evalConfig env (ConfigNumber n) = Right (CNumber n)
evalConfig env (ConfigBoolean b) = Right (CBoolean b)
evalConfig env (ConfigList exprs) = do
    vals <- mapM (evalConfig env) exprs
    return (CList vals)
evalConfig env (ConfigSection name pairs) = do
    vals <- mapM (\(k, v) -> do
        val <- evalConfig env v
        return (k, val)) pairs
    return (CSection (fromList vals))
evalConfig env (ConfigInclude path) = do
    -- 실제 구현에서는 파일을 읽어서 파싱
    return (CString ("Included: " `mappend` path))
evalConfig env (ConfigCondition cond thenExpr elseExpr) = do
    condVal <- evalConfig env cond
    case condVal of
        CBoolean True -> evalConfig env thenExpr
        CBoolean False -> evalConfig env elseExpr
        _ -> Left "Condition must be boolean"

-- 5. 설정 값 출력
showConfigValue :: ConfigValue -> Text
showConfigValue (CString s) = "\"" `mappend` s `mappend` "\""
showConfigValue (CNumber n) = pack (show n)
showConfigValue (CBoolean b) = if b then "true" else "false"
showConfigValue (CList vals) = "[" `mappend` pack (intercalate ", " (map (unpack . showConfigValue) vals)) `mappend` "]"
showConfigValue (CSection m) = "{" `mappend` pack (intercalate ", " (map (\(k, v) -> unpack k `mappend` ": " `mappend` unpack (showConfigValue v)) (Map.toList m))) `mappend` "}"

-- 6. 설정 검증
validateConfig :: ConfigEnv -> [ConfigExpr] -> Either Text ConfigEnv
validateConfig env [] = Right env
validateConfig env (expr : rest) = do
    val <- evalConfig env expr
    case expr of
        ConfigSection name pairs -> do
            let newSection = Map.fromList (map (\(k, v) -> (k, val)) pairs)
            let newEnv = env { configSections = Map.insert name newSection (configSections env) }
            validateConfig newEnv rest
        _ -> validateConfig env rest

-- 7. 설정 파일 예제
configExample :: Text
configExample = pack $ unlines
    [ "# Application Configuration"
    , "app_name = \"My Application\""
    , "version = \"1.0.0\""
    , "debug = true"
    , "port = 8080"
    , "timeout = 30.0"
    , ""
    , "# Database Configuration"
    , "database_host = \"localhost\""
    , "database_port = 5432"
    , "database_name = \"myapp\""
    , "database_ssl = true"
    , ""
    , "# Features"
    , "features = [\"auth\", \"logging\", \"metrics\"]"
    , ""
    , "# Logging Configuration"
    , "log_level = \"info\""
    , "log_file = \"/var/log/app.log\""
    , "log_max_size = 100"
    ]

-- 8. 테스트 함수
testConfigPDSL :: IO ()
testConfigPDSL = do
    putStrLn "=== Config PDSL Examples ==="
    putStrLn ""
    
    putStrLn "Configuration file:"
    putStrLn $ unpack configExample
    putStrLn ""
    
    case parseConfig configExample of
        Left err -> putStrLn $ "Parse error: " ++ unpack err
        Right exprs -> do
            putStrLn "Parsed configuration:"
            mapM_ (putStrLn . show) exprs
            putStrLn ""
            
            let env = ConfigEnv Map.empty Map.empty []
            case validateConfig env exprs of
                Left err -> putStrLn $ "Validation error: " ++ unpack err
                Right finalEnv -> do
                    putStrLn "Validated configuration:"
                    putStrLn $ "Sections: " ++ show (Map.keys (configSections finalEnv))
                    putStrLn ""
                    
                    -- 개별 값 출력
                    putStrLn "Individual values:"
                    case Map.lookup "app_name" (configSections finalEnv) of
                        Just section -> case Map.lookup "app_name" section of
                            Just (CString name) -> putStrLn $ "App name: " ++ unpack name
                            _ -> putStrLn "App name not found"
                        _ -> putStrLn "App name section not found"
                    
                    case Map.lookup "port" (configSections finalEnv) of
                        Just section -> case Map.lookup "port" section of
                            Just (CNumber port) -> putStrLn $ "Port: " ++ show port
                            _ -> putStrLn "Port not found"
                        _ -> putStrLn "Port section not found"

-- 9. 유틸리티 함수들
trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = case break (== c) s of
    (x, []) -> [x]
    (x, _ : xs) -> x : splitOn c xs

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
