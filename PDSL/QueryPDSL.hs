{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module QueryPDSL where

-- Haskell로 구현한 쿼리 PDSL
-- 이 모듈은 SQL과 유사한 쿼리를 파싱하고 실행하는 PDSL을 구현합니다

import Data.Map (Map, fromList, lookup, insert)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import Data.List (intercalate, sortBy)
import Control.Monad (when, unless)

-- 1. 쿼리 PDSL 문법 정의
data QueryExpr
    = Select [Text] Text (Maybe WhereClause) (Maybe OrderClause) (Maybe LimitClause)
    | Insert Text [(Text, ValueExpr)]
    | Update Text [(Text, ValueExpr)] (Maybe WhereClause)
    | Delete Text (Maybe WhereClause)
    deriving (Show, Eq)

data WhereClause = WhereClause Condition deriving (Show, Eq)
data OrderClause = OrderClause [Text] deriving (Show, Eq)
data LimitClause = LimitClause Int deriving (Show, Eq)

data Condition
    = Equal Text ValueExpr
    | NotEqual Text ValueExpr
    | LessThan Text ValueExpr
    | GreaterThan Text ValueExpr
    | LessEqual Text ValueExpr
    | GreaterEqual Text ValueExpr
    | Like Text ValueExpr
    | In Text [ValueExpr]
    | And Condition Condition
    | Or Condition Condition
    | Not Condition
    deriving (Show, Eq)

data ValueExpr
    = VString Text
    | VNumber Double
    | VBoolean Bool
    | VColumn Text
    | VNull
    deriving (Show, Eq)

-- 2. 데이터베이스 환경
data DatabaseEnv = DatabaseEnv
    { tables :: Map Text Table
    , currentQuery :: Maybe QueryExpr
    } deriving (Show)

data Table = Table
    { tableName :: Text
    , columns :: [Column]
    , rows :: [Row]
    } deriving (Show)

data Column = Column
    { columnName :: Text
    , columnType :: Text
    } deriving (Show, Eq)

data Row = Row
    { rowData :: Map Text Value
    } deriving (Show, Eq)

data Value
    = StringValue Text
    | NumberValue Double
    | BooleanValue Bool
    | NullValue
    deriving (Show, Eq)

-- 3. 쿼리 PDSL 파서
parseQuery :: Text -> Either Text QueryExpr
parseQuery input = case words (unpack input) of
    ("SELECT" : rest) -> parseSelect rest
    ("INSERT" : rest) -> parseInsert rest
    ("UPDATE" : rest) -> parseUpdate rest
    ("DELETE" : rest) -> parseDelete rest
    _ -> Left "Invalid query: must start with SELECT, INSERT, UPDATE, or DELETE"

parseSelect :: [String] -> Either Text QueryExpr
parseSelect tokens = do
    (columns, tokens') <- parseColumns tokens
    case tokens' of
        ("FROM" : tableName : rest) -> do
            (whereClause, tokens'') <- parseWhere rest
            (orderClause, tokens''') <- parseOrder tokens''
            (limitClause, tokens'''') <- parseLimit tokens'''
            return (Select columns (pack tableName) whereClause orderClause limitClause)
        _ -> Left "Missing FROM clause"

parseColumns :: [String] -> Either Text ([Text], [String])
parseColumns ("*" : rest) = Right ([pack "*"], rest)
parseColumns tokens = parseColumnList tokens []

parseColumnList :: [String] -> [Text] -> Either Text ([Text], [String])
parseColumnList [] acc = Right (reverse acc, [])
parseColumnList ("FROM" : rest) acc = Right (reverse acc, "FROM" : rest)
parseColumnList ("," : rest) acc = parseColumnList rest acc
parseColumnList (col : rest) acc = parseColumnList rest (pack col : acc)

parseWhere :: [String] -> Either Text (Maybe WhereClause, [String])
parseWhere ("WHERE" : rest) = do
    (condition, tokens') <- parseCondition rest
    return (Just (WhereClause condition), tokens')
parseWhere tokens = Right (Nothing, tokens)

parseCondition :: [String] -> Either Text (Condition, [String])
parseCondition tokens = parseOrCondition tokens

parseOrCondition :: [String] -> Either Text (Condition, [String])
parseOrCondition tokens = do
    (left, tokens') <- parseAndCondition tokens
    case tokens' of
        ("OR" : rest) -> do
            (right, tokens'') <- parseOrCondition rest
            return (Or left right, tokens'')
        _ -> return (left, tokens')

parseAndCondition :: [String] -> Either Text (Condition, [String])
parseAndCondition tokens = do
    (left, tokens') <- parseSimpleCondition tokens
    case tokens' of
        ("AND" : rest) -> do
            (right, tokens'') <- parseAndCondition rest
            return (And left right, tokens'')
        _ -> return (left, tokens')

parseSimpleCondition :: [String] -> Either Text (Condition, [String])
parseSimpleCondition (col : "=" : val : rest) = do
    value <- parseValue val
    return (Equal (pack col) value, rest)
parseSimpleCondition (col : "!=" : val : rest) = do
    value <- parseValue val
    return (NotEqual (pack col) value, rest)
parseSimpleCondition (col : "<" : val : rest) = do
    value <- parseValue val
    return (LessThan (pack col) value, rest)
parseSimpleCondition (col : ">" : val : rest) = do
    value <- parseValue val
    return (GreaterThan (pack col) value, rest)
parseSimpleCondition (col : "<=" : val : rest) = do
    value <- parseValue val
    return (LessEqual (pack col) value, rest)
parseSimpleCondition (col : ">=" : val : rest) = do
    value <- parseValue val
    return (GreaterEqual (pack col) value, rest)
parseSimpleCondition ("NOT" : rest) = do
    (condition, tokens') <- parseSimpleCondition rest
    return (Not condition, tokens')
parseSimpleCondition _ = Left "Invalid condition"

parseValue :: String -> Either Text ValueExpr
parseValue val = case val of
    x | head x == '"' && last x == '"' -> 
        Right (VString (pack (init (tail x))))
    x | x == "true" -> Right (VBoolean True)
    x | x == "false" -> Right (VBoolean False)
    x | x == "NULL" -> Right VNull
    x | all isDigit x || (head x == '-' && all isDigit (tail x)) -> 
        Right (VNumber (read x))
    x -> Right (VColumn (pack x))

parseOrder :: [String] -> Either Text (Maybe OrderClause, [String])
parseOrder ("ORDER" : "BY" : rest) = do
    (columns, tokens') <- parseColumnList rest []
    return (Just (OrderClause columns), tokens')
parseOrder tokens = Right (Nothing, tokens)

parseLimit :: [String] -> Either Text (Maybe LimitClause, [String])
parseLimit ("LIMIT" : num : rest) = do
    let limit = read num
    return (Just (LimitClause limit), rest)
parseLimit tokens = Right (Nothing, tokens)

parseInsert :: [String] -> Either Text QueryExpr
parseInsert ("INTO" : tableName : "VALUES" : rest) = do
    -- 간단한 구현
    return (Insert (pack tableName) [])
parseInsert _ = Left "Invalid INSERT statement"

parseUpdate :: [String] -> Either Text QueryExpr
parseUpdate (tableName : "SET" : rest) = do
    -- 간단한 구현
    return (Update (pack tableName) [] Nothing)
parseUpdate _ = Left "Invalid UPDATE statement"

parseDelete :: [String] -> Either Text QueryExpr
parseDelete ("FROM" : tableName : rest) = do
    (whereClause, _) <- parseWhere rest
    return (Delete (pack tableName) whereClause)
parseDelete _ = Left "Invalid DELETE statement"

-- 4. 쿼리 실행기
executeQuery :: DatabaseEnv -> QueryExpr -> Either Text [Row]
executeQuery env (Select columns tableName whereClause orderClause limitClause) = do
    table <- case Map.lookup tableName (tables env) of
        Just t -> Right t
        Nothing -> Left ("Table not found: " `mappend` tableName)
    
    let filteredRows = case whereClause of
            Just (WhereClause condition) -> filter (evaluateCondition condition) (rows table)
            Nothing -> rows table
    
    let sortedRows = case orderClause of
            Just (OrderClause orderColumns) -> sortRows orderColumns filteredRows
            Nothing -> filteredRows
    
    let limitedRows = case limitClause of
            Just (LimitClause limit) -> take limit sortedRows
            Nothing -> sortedRows
    
    return limitedRows

executeQuery env (Insert tableName values) = do
    -- INSERT 구현
    return []
executeQuery env (Update tableName values whereClause) = do
    -- UPDATE 구현
    return []
executeQuery env (Delete tableName whereClause) = do
    -- DELETE 구현
    return []

evaluateCondition :: Condition -> Row -> Bool
evaluateCondition (Equal col val) row = case (getColumnValue col row, evalValue val row) of
    (StringValue a, StringValue b) -> a == b
    (NumberValue a, NumberValue b) -> a == b
    (BooleanValue a, BooleanValue b) -> a == b
    _ -> False
evaluateCondition (NotEqual col val) row = not (evaluateCondition (Equal col val) row)
evaluateCondition (LessThan col val) row = case (getColumnValue col row, evalValue val row) of
    (NumberValue a, NumberValue b) -> a < b
    _ -> False
evaluateCondition (GreaterThan col val) row = case (getColumnValue col row, evalValue val row) of
    (NumberValue a, NumberValue b) -> a > b
    _ -> False
evaluateCondition (And left right) row = evaluateCondition left row && evaluateCondition right row
evaluateCondition (Or left right) row = evaluateCondition left row || evaluateCondition right row
evaluateCondition (Not condition) row = not (evaluateCondition condition row)
evaluateCondition _ _ = False

getColumnValue :: Text -> Row -> Value
getColumnValue col row = case Map.lookup col (rowData row) of
    Just val -> val
    Nothing -> NullValue

evalValue :: ValueExpr -> Row -> Value
evalValue (VString s) _ = StringValue s
evalValue (VNumber n) _ = NumberValue n
evalValue (VBoolean b) _ = BooleanValue b
evalValue (VColumn col) row = getColumnValue col row
evalValue VNull _ = NullValue

sortRows :: [Text] -> [Row] -> [Row]
sortRows columns rows = sortBy (compareRows columns) rows

compareRows :: [Text] -> Row -> Row -> Ordering
compareRows [] _ _ = EQ
compareRows (col : rest) row1 row2 = case (getColumnValue col row1, getColumnValue col row2) of
    (StringValue a, StringValue b) -> case compare a b of
        EQ -> compareRows rest row1 row2
        other -> other
    (NumberValue a, NumberValue b) -> case compare a b of
        EQ -> compareRows rest row1 row2
        other -> other
    _ -> compareRows rest row1 row2

-- 5. 테스트 데이터 생성
createTestDatabase :: DatabaseEnv
createTestDatabase = DatabaseEnv
    { tables = fromList
        [ ("users", Table "users" 
            [ Column "id" "INT"
            , Column "name" "VARCHAR"
            , Column "age" "INT"
            , Column "email" "VARCHAR"
            ] 
            [ Row (fromList [("id", NumberValue 1), ("name", StringValue "John"), ("age", NumberValue 25), ("email", StringValue "john@example.com")])
            , Row (fromList [("id", NumberValue 2), ("name", StringValue "Jane"), ("age", NumberValue 30), ("email", StringValue "jane@example.com")])
            , Row (fromList [("id", NumberValue 3), ("name", StringValue "Bob"), ("age", NumberValue 35), ("email", StringValue "bob@example.com")])
            ])
        ]
    , currentQuery = Nothing
    }

-- 6. 테스트 함수
testQueryPDSL :: IO ()
testQueryPDSL = do
    putStrLn "=== Query PDSL Examples ==="
    putStrLn ""
    
    let env = createTestDatabase
    
    putStrLn "Test database created with users table"
    putStrLn ""
    
    -- SELECT 쿼리 테스트
    putStrLn "Example 1: SELECT * FROM users"
    case parseQuery "SELECT * FROM users" of
        Left err -> putStrLn $ "Parse error: " ++ unpack err
        Right query -> case executeQuery env query of
            Left err -> putStrLn $ "Execution error: " ++ unpack err
            Right rows -> do
                putStrLn $ "Found " ++ show (length rows) ++ " rows"
                mapM_ (putStrLn . show) rows
    putStrLn ""
    
    -- WHERE 절 테스트
    putStrLn "Example 2: SELECT * FROM users WHERE age > 25"
    case parseQuery "SELECT * FROM users WHERE age > 25" of
        Left err -> putStrLn $ "Parse error: " ++ unpack err
        Right query -> case executeQuery env query of
            Left err -> putStrLn $ "Execution error: " ++ unpack err
            Right rows -> do
                putStrLn $ "Found " ++ show (length rows) ++ " rows"
                mapM_ (putStrLn . show) rows
    putStrLn ""
    
    -- 복잡한 WHERE 절 테스트
    putStrLn "Example 3: SELECT * FROM users WHERE age > 25 AND name = \"Jane\""
    case parseQuery "SELECT * FROM users WHERE age > 25 AND name = \"Jane\"" of
        Left err -> putStrLn $ "Parse error: " ++ unpack err
        Right query -> case executeQuery env query of
            Left err -> putStrLn $ "Execution error: " ++ unpack err
            Right rows -> do
                putStrLn $ "Found " ++ show (length rows) ++ " rows"
                mapM_ (putStrLn . show) rows
    putStrLn ""
    
    -- 추가 쿼리 예제들
    putStrLn "Example 4: SELECT name, age FROM users WHERE age > 25"
    case parseQuery "SELECT name, age FROM users WHERE age > 25" of
        Left err -> putStrLn $ "Parse error: " ++ unpack err
        Right query -> case executeQuery env query of
            Left err -> putStrLn $ "Execution error: " ++ unpack err
            Right rows -> do
                putStrLn $ "Found " ++ show (length rows) ++ " rows"
                mapM_ (putStrLn . show) rows
    putStrLn ""
    
    putStrLn "Example 5: SELECT * FROM users WHERE age < 30"
    case parseQuery "SELECT * FROM users WHERE age < 30" of
        Left err -> putStrLn $ "Parse error: " ++ unpack err
        Right query -> case executeQuery env query of
            Left err -> putStrLn $ "Execution error: " ++ unpack err
            Right rows -> do
                putStrLn $ "Found " ++ show (length rows) ++ " rows"
                mapM_ (putStrLn . show) rows
    putStrLn ""

-- 7. 유틸리티 함수들
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
