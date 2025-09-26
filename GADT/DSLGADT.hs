{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module DSLGADT where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

-- 1. DSL에서 GADT 활용: 타입 안전한 SQL 쿼리

-- 쿼리 결과 타입을 나타내는 태그
data QueryType = Select | Insert | Update | Delete

-- GADT를 사용한 타입 안전한 SQL 쿼리
data SQLQuery (q :: QueryType) where
    -- SELECT 쿼리: 결과 타입이 [a]
    SelectQuery :: [Text] -> Text -> Maybe Condition -> SQLQuery Select
    
    -- INSERT 쿼리: 결과 타입이 Int (영향받은 행 수)
    InsertQuery :: Text -> [(Text, Value)] -> SQLQuery Insert
    
    -- UPDATE 쿼리: 결과 타입이 Int (영향받은 행 수)
    UpdateQuery :: Text -> [(Text, Value)] -> Maybe Condition -> SQLQuery Update
    
    -- DELETE 쿼리: 결과 타입이 Int (삭제된 행 수)
    DeleteQuery :: Text -> Maybe Condition -> SQLQuery Delete

-- 조건식
data Condition where
    Eq :: Text -> Value -> Condition
    Lt :: Text -> Value -> Condition
    Gt :: Text -> Value -> Condition
    And :: Condition -> Condition -> Condition
    Or :: Condition -> Condition -> Condition

-- 값 타입
data Value where
    IntValue :: Int -> Value
    StringValue :: Text -> Value
    BoolValue :: Bool -> Value

-- 2. 타입 안전한 쿼리 실행 함수
executeQuery :: SQLQuery q -> QueryResult q
executeQuery (SelectQuery cols table cond) = 
    [pack "row1", pack "row2", pack "row3"]  -- 실제로는 DB에서 조회
executeQuery (InsertQuery table values) = 
    1  -- 실제로는 삽입된 행 수
executeQuery (UpdateQuery table values cond) = 
    2  -- 실제로는 업데이트된 행 수
executeQuery (DeleteQuery table cond) = 
    1  -- 실제로는 삭제된 행 수

-- 쿼리 결과 타입 (타입 패밀리 사용)
type family QueryResult (q :: QueryType) where
    QueryResult Select = [Text]
    QueryResult Insert = Int
    QueryResult Update = Int
    QueryResult Delete = Int

-- 3. DSL 빌더 함수들
select :: [Text] -> Text -> SQLQuery Select
select cols table = SelectQuery cols table Nothing

where_ :: SQLQuery Select -> Condition -> SQLQuery Select
where_ (SelectQuery cols table _) cond = SelectQuery cols table (Just cond)

insert :: Text -> [(Text, Value)] -> SQLQuery Insert
insert = InsertQuery

update :: Text -> [(Text, Value)] -> SQLQuery Update
update table values = UpdateQuery table values Nothing

delete :: Text -> SQLQuery Delete
delete table = DeleteQuery table Nothing

-- 4. 조건 빌더
(==.) :: Text -> Value -> Condition
(==.) = Eq

(<.) :: Text -> Value -> Condition
(<.) = Lt

(>.) :: Text -> Value -> Condition
(>.) = Gt

(.&&.) :: Condition -> Condition -> Condition
(.&&.) = And

(.||.) :: Condition -> Condition -> Condition
(.||.) = Or

-- 5. 값 생성자
int :: Int -> Value
int = IntValue

string :: Text -> Value
string = StringValue

bool :: Bool -> Value
bool = BoolValue

-- 6. 테스트 함수
testDSLGADT :: IO ()
testDSLGADT = do
    putStrLn "=== DSL with GADT Examples ==="
    
    -- 타입 안전한 쿼리 생성
    let selectQuery = select [pack "name", pack "age"] (pack "users")
                    `where_` (pack "age" >. int 18)
    
    let insertQuery = insert (pack "users") 
                    [(pack "name", string (pack "John")), (pack "age", int 25)]
    
    let updateQuery = update (pack "users") 
                    [(pack "age", int 26)]
    
    let deleteQuery = delete (pack "users")
    
    putStrLn "Generated queries:"
    putStrLn "SELECT: SelectQuery with columns and conditions"
    putStrLn "INSERT: InsertQuery with table and values"
    putStrLn "UPDATE: UpdateQuery with table and values"
    putStrLn "DELETE: DeleteQuery with table"
    
    putStrLn "\nGADT in DSL의 장점:"
    putStrLn "- 쿼리 타입에 따라 결과 타입이 자동으로 결정"
    putStrLn "- 잘못된 쿼리 조합은 컴파일 시점에 감지"
    putStrLn "- 타입 안전한 쿼리 실행"
