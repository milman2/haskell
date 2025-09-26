{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module ConfigGADT where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Map (Map, fromList, insert, lookup)
import qualified Data.Map as Map

-- 1. 설정 타입을 나타내는 태그
data ConfigType = StringType | IntType | BoolType | ListType | SectionType

-- GADT를 사용한 타입 안전한 설정
data ConfigValue (t :: ConfigType) where
    -- 문자열 설정
    StringConfig :: Text -> ConfigValue StringType
    
    -- 정수 설정
    IntConfig :: Int -> ConfigValue IntType
    
    -- 불린 설정
    BoolConfig :: Bool -> ConfigValue BoolType
    
    -- 리스트 설정
    ListConfig :: [Text] -> ConfigValue ListType
    
    -- 섹션 설정 (중첩된 설정)
    SectionConfig :: Map Text (SomeConfigValue) -> ConfigValue SectionType

-- 타입 지우기 (존재 타입)
data SomeConfigValue where
    SomeConfigValue :: ConfigValue t -> SomeConfigValue

-- 2. 타입 안전한 설정 조회
getStringConfig :: ConfigValue StringType -> Text
getStringConfig (StringConfig s) = s

getIntConfig :: ConfigValue IntType -> Int
getIntConfig (IntConfig i) = i

getBoolConfig :: ConfigValue BoolType -> Bool
getBoolConfig (BoolConfig b) = b

getListConfig :: ConfigValue ListType -> [Text]
getListConfig (ListConfig l) = l

getSectionConfig :: ConfigValue SectionType -> Map Text SomeConfigValue
getSectionConfig (SectionConfig s) = s

-- 3. 설정 빌더 DSL
class ConfigBuilder a where
    build :: a -> SomeConfigValue

-- 기본 설정 빌더들
instance ConfigBuilder (ConfigValue StringType) where
    build = SomeConfigValue

instance ConfigBuilder (ConfigValue IntType) where
    build = SomeConfigValue

instance ConfigBuilder (ConfigValue BoolType) where
    build = SomeConfigValue

instance ConfigBuilder (ConfigValue ListType) where
    build = SomeConfigValue

instance ConfigBuilder (ConfigValue SectionType) where
    build = SomeConfigValue

-- 4. 설정 생성 헬퍼 함수들
string :: Text -> ConfigValue StringType
string = StringConfig

int :: Int -> ConfigValue IntType
int = IntConfig

bool :: Bool -> ConfigValue BoolType
bool = BoolConfig

list :: [Text] -> ConfigValue ListType
list = ListConfig

section :: [(Text, SomeConfigValue)] -> ConfigValue SectionType
section pairs = SectionConfig (fromList pairs)

-- 5. 타입 안전한 설정 검증
validateConfig :: SomeConfigValue -> Either Text SomeConfigValue
validateConfig (SomeConfigValue (StringConfig s)) = 
    if T.null s then Left (pack "String config cannot be empty") 
    else Right (SomeConfigValue (StringConfig s))
validateConfig (SomeConfigValue (IntConfig i)) = 
    if i < 0 then Left (pack "Int config cannot be negative") 
    else Right (SomeConfigValue (IntConfig i))
validateConfig (SomeConfigValue (BoolConfig b)) = 
    Right (SomeConfigValue (BoolConfig b))
validateConfig (SomeConfigValue (ListConfig l)) = 
    if null l then Left (pack "List config cannot be empty") 
    else Right (SomeConfigValue (ListConfig l))
validateConfig (SomeConfigValue (SectionConfig s)) = 
    if Map.null s then Left (pack "Section config cannot be empty") 
    else Right (SomeConfigValue (SectionConfig s))

-- 6. 설정 예제
appConfig :: ConfigValue SectionType
appConfig = section
    [ (pack "app_name", build (string (pack "MyApp")))
    , (pack "version", build (int 1))
    , (pack "debug", build (bool True))
    , (pack "features", build (list [pack "auth", pack "logging"]))
    , (pack "database", build (section
        [ (pack "host", build (string (pack "localhost")))
        , (pack "port", build (int 5432))
        , (pack "ssl", build (bool True))
        ]))
    ]

-- 7. 테스트 함수
testConfigGADT :: IO ()
testConfigGADT = do
    putStrLn "=== Config DSL with GADT Examples ==="
    
    putStrLn "생성된 설정:"
    putStrLn "App Config: SectionConfig with nested configuration"
    
    putStrLn "\n타입 안전한 설정 조회:"
    case getSectionConfig appConfig of
        sectionMap -> do
            case Map.lookup (pack "app_name") sectionMap of
                Just (SomeConfigValue (StringConfig name)) -> 
                    putStrLn $ "App name: " ++ unpack name
                _ -> putStrLn "App name not found"
            
            case Map.lookup (pack "version") sectionMap of
                Just (SomeConfigValue (IntConfig version)) -> 
                    putStrLn $ "Version: " ++ show version
                _ -> putStrLn "Version not found"
    
    putStrLn "\nGADT in Config DSL의 장점:"
    putStrLn "- 설정 타입에 따른 타입 안전성"
    putStrLn "- 잘못된 타입 조회는 컴파일 시점에 감지"
    putStrLn "- 중첩된 설정 구조의 타입 안전성"
    putStrLn "- 설정 검증의 타입 안전성"
