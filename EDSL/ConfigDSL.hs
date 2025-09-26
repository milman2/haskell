{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ConfigDSL where

-- 간단한 설정 파일 EDSL 예제
-- 이 EDSL은 설정을 Haskell 코드로 작성할 수 있게 해줍니다

import Data.Map (Map, fromList, lookup)
import qualified Data.Map as Map
import Data.List (intercalate)

-- 1. 설정 값의 데이터 타입
data ConfigValue
    = CString String
    | CInt Int
    | CDouble Double
    | CBool Bool
    | CList [ConfigValue]
    | CMap (Map String ConfigValue)
    deriving (Show, Eq)

-- 2. 설정 빌더 모나드
newtype ConfigBuilder a = ConfigBuilder (Map String ConfigValue -> (a, Map String ConfigValue))

instance Functor ConfigBuilder where
    fmap f (ConfigBuilder g) = ConfigBuilder $ \env ->
        let (a, env') = g env
        in (f a, env')

instance Applicative ConfigBuilder where
    pure x = ConfigBuilder $ \env -> (x, env)
    ConfigBuilder f <*> ConfigBuilder g = ConfigBuilder $ \env ->
        let (h, env') = f env
            (a, env'') = g env'
        in (h a, env'')

instance Monad ConfigBuilder where
    return x = ConfigBuilder $ \env -> (x, env)
    ConfigBuilder f >>= g = ConfigBuilder $ \env ->
        let (a, env') = f env
            ConfigBuilder h = g a
        in h env'

-- 3. 설정 빌더 함수들
set :: String -> ConfigValue -> ConfigBuilder ()
set key value = ConfigBuilder $ \env -> ((), Map.insert key value env)

get :: String -> ConfigBuilder (Maybe ConfigValue)
get key = ConfigBuilder $ \env -> (Map.lookup key env, env)

-- 4. 설정 값 생성 함수들
string :: String -> ConfigValue
string = CString

int :: Int -> ConfigValue
int = CInt

double :: Double -> ConfigValue
double = CDouble

bool :: Bool -> ConfigValue
bool = CBool

list :: [ConfigValue] -> ConfigValue
list = CList

map_ :: [(String, ConfigValue)] -> ConfigValue
map_ = CMap . fromList

-- 5. 설정 실행 함수
runConfig :: ConfigBuilder a -> (a, Map String ConfigValue)
runConfig (ConfigBuilder f) = f Map.empty

-- 6. 설정을 JSON 형태로 출력하는 함수
configToJSON :: ConfigValue -> String
configToJSON (CString s) = "\"" ++ s ++ "\""
configToJSON (CInt i) = show i
configToJSON (CDouble d) = show d
configToJSON (CBool b) = if b then "true" else "false"
configToJSON (CList values) = "[" ++ intercalate ", " (map configToJSON values) ++ "]"
configToJSON (CMap m) = "{" ++ intercalate ", " (map (\(k, v) -> "\"" ++ k ++ "\": " ++ configToJSON v) (Map.toList m)) ++ "}"

-- 7. 설정 예시
appConfig :: ConfigBuilder ()
appConfig = do
    set "app_name" (string "My Application")
    set "version" (string "1.0.0")
    set "debug" (bool True)
    set "port" (int 8080)
    set "timeout" (double 30.0)
    set "features" (list [string "auth", string "logging", string "metrics"])
    set "database" (map_ [
        ("host", string "localhost"),
        ("port", int 5432),
        ("name", string "myapp"),
        ("ssl", bool True)
        ])
    set "logging" (map_ [
        ("level", string "info"),
        ("file", string "/var/log/app.log"),
        ("max_size", int 100)
        ])

-- 8. 설정 읽기 예시
readConfig :: ConfigBuilder String
readConfig = do
    appName <- get "app_name"
    port <- get "port"
    debug <- get "debug"
    return $ case (appName, port, debug) of
        (Just (CString name), Just (CInt p), Just (CBool d)) ->
            "App: " ++ name ++ ", Port: " ++ show p ++ ", Debug: " ++ show d
        _ -> "Configuration incomplete"

-- 9. 테스트 함수
testConfig :: IO ()
testConfig = do
    putStrLn "=== Config DSL Examples ==="
    putStrLn ""
    
    let (_, config) = runConfig appConfig
    let (result, _) = runConfig readConfig
    
    putStrLn "Configuration as JSON:"
    putStrLn $ configToJSON (CMap config)
    putStrLn ""
    
    putStrLn "Read configuration:"
    putStrLn result
    putStrLn ""
    
    putStrLn "Individual values:"
    putStrLn $ "App name: " ++ case Map.lookup "app_name" config of
        Just (CString name) -> name
        _ -> "Not found"
    putStrLn $ "Port: " ++ case Map.lookup "port" config of
        Just (CInt port) -> show port
        _ -> "Not found"
    putStrLn $ "Debug: " ++ case Map.lookup "debug" config of
        Just (CBool debug) -> show debug
        _ -> "Not found"
