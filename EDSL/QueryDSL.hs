{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module QueryDSL where

-- 간단한 데이터베이스 쿼리 EDSL 예제
-- 이 EDSL은 SQL과 유사한 쿼리를 Haskell 코드로 작성할 수 있게 해줍니다

import Data.List (intercalate)

-- 1. 데이터 타입 정의
data Table = Table String [Column]
data Column = Column String String  -- (name, type)
data Value = VString String | VInt Int | VDouble Double | VNull
data Condition = Eq Column Value | Lt Column Value | Gt Column Value | And Condition Condition | Or Condition Condition
data Query = Select [Column] Table (Maybe Condition) (Maybe [Column]) (Maybe Int)

-- 2. 테이블 정의 예시
users :: Table
users = Table "users" [
    Column "id" "INT",
    Column "name" "VARCHAR",
    Column "age" "INT",
    Column "email" "VARCHAR"
    ]

products :: Table
products = Table "products" [
    Column "id" "INT",
    Column "name" "VARCHAR",
    Column "price" "DOUBLE",
    Column "category" "VARCHAR"
    ]

-- 3. 쿼리 빌더 함수들
select :: [Column] -> Table -> Query
select cols table = Select cols table Nothing Nothing Nothing

where_ :: Query -> Condition -> Query
where_ (Select cols table _ orderBy limit) cond = 
    Select cols table (Just cond) orderBy limit

orderBy_ :: Query -> [Column] -> Query
orderBy_ (Select cols table cond _ limit) orderCols = 
    Select cols table cond (Just orderCols) limit

limit_ :: Query -> Int -> Query
limit_ (Select cols table cond orderBy _) n = 
    Select cols table cond orderBy (Just n)

-- 4. 조건 빌더 함수들
(==.) :: Column -> Value -> Condition
(==.) = Eq

(<.) :: Column -> Value -> Condition
(<.) = Lt

(>.) :: Column -> Value -> Condition
(>.) = Gt

(&&.) :: Condition -> Condition -> Condition
(&&.) = And

(||.) :: Condition -> Condition -> Condition
(||.) = Or

-- 5. SQL 생성 함수
toSQL :: Query -> String
toSQL (Select cols table cond orderBy limit) = 
    let selectClause = "SELECT " ++ intercalate ", " (map (\(Column name _) -> name) cols)
        fromClause = "FROM " ++ case table of Table name _ -> name
        whereClause = case cond of
            Nothing -> ""
            Just c -> " WHERE " ++ conditionToSQL c
        orderClause = case orderBy of
            Nothing -> ""
            Just cols -> " ORDER BY " ++ intercalate ", " (map (\(Column name _) -> name) cols)
        limitClause = case limit of
            Nothing -> ""
            Just n -> " LIMIT " ++ show n
    in selectClause ++ fromClause ++ whereClause ++ orderClause ++ limitClause

conditionToSQL :: Condition -> String
conditionToSQL (Eq (Column name _) val) = name ++ " = " ++ valueToSQL val
conditionToSQL (Lt (Column name _) val) = name ++ " < " ++ valueToSQL val
conditionToSQL (Gt (Column name _) val) = name ++ " > " ++ valueToSQL val
conditionToSQL (And c1 c2) = "(" ++ conditionToSQL c1 ++ " AND " ++ conditionToSQL c2 ++ ")"
conditionToSQL (Or c1 c2) = "(" ++ conditionToSQL c1 ++ " OR " ++ conditionToSQL c2 ++ ")"

valueToSQL :: Value -> String
valueToSQL (VString s) = "'" ++ s ++ "'"
valueToSQL (VInt i) = show i
valueToSQL (VDouble d) = show d
valueToSQL VNull = "NULL"

-- 6. 사용 예시
example1 :: Query
example1 = select [Column "name" "VARCHAR", Column "age" "INT"] users
    `where_` (Column "age" "INT" >. VInt 18)
    `orderBy_` [Column "name" "VARCHAR"]

example2 :: Query
example2 = select [Column "name" "VARCHAR", Column "price" "DOUBLE"] products
    `where_` ((Column "price" "DOUBLE" <. VDouble 100.0) &&. (Column "category" "VARCHAR" ==. VString "electronics"))
    `limit_` 10

example3 :: Query
example3 = select [Column "id" "INT", Column "name" "VARCHAR", Column "email" "VARCHAR"] users
    `where_` ((Column "age" "INT" >. VInt 25) ||. (Column "name" "VARCHAR" ==. VString "admin"))

-- 7. 테스트 함수
testQuery :: IO ()
testQuery = do
    putStrLn "=== Query DSL Examples ==="
    putStrLn ""
    
    putStrLn "Example 1: Select name, age from users where age > 18 order by name"
    putStrLn $ "SQL: " ++ toSQL example1
    putStrLn ""
    
    putStrLn "Example 2: Select name, price from products where price < 100.0 and category = 'electronics' limit 10"
    putStrLn $ "SQL: " ++ toSQL example2
    putStrLn ""
    
    putStrLn "Example 3: Select id, name, email from users where age > 25 or name = 'admin'"
    putStrLn $ "SQL: " ++ toSQL example3
    putStrLn ""
