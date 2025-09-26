{-# LANGUAGE OverloadedStrings #-}

module CodeGenTest where

import Protobuf.CodeGen
import Protobuf.Types
import Protobuf.AST
import Data.Text (Text, pack)
import Test.Hspec
import Test.QuickCheck

-- 1. 기본 코드 생성 테스트

testBasicCodeGeneration :: IO ()
testBasicCodeGeneration = hspec $ do
    describe "Protobuf Code Generation" $ do
        it "should generate simple message type" $ do
            let msg = Message "Person" 
                    [ Field Required (ScalarType StringType) "name" 1 []
                    , Field Required (ScalarType Int32Type) "age" 2 []
                    ] [] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
            code `shouldContain` "data Person = Person"
            code `shouldContain` "name :: Text"
            code `shouldContain` "age :: Int32"
            code `shouldContain` "deriving (Show, Eq, Generic)"
        
        it "should generate enum type" $ do
            let enum = Enum "Color" 
                    [ EnumValue "RED" 0 []
                    , EnumValue "GREEN" 1 []
                    , EnumValue "BLUE" 2 []
                    ] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileEnum enum]
            code `shouldContain` "data Color = RED | GREEN | BLUE"
            code `shouldContain` "deriving (Show, Eq, Generic)"
        
        it "should generate service class" $ do
            let service = Service "UserService" 
                    [ Method "GetUser" "Int32" "User" []
                    , Method "CreateUser" "User" "Int32" []
                    ] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileService service]
            code `shouldContain` "class UserService m where"
            code `shouldContain` "GetUser :: Int32 -> m User"
            code `shouldContain` "CreateUser :: User -> m Int32"

-- 2. 복잡한 코드 생성 테스트

testComplexCodeGeneration :: IO ()
testComplexCodeGeneration = hspec $ do
    describe "Complex Code Generation" $ do
        it "should generate nested message" $ do
            let nestedMsg = Message "PhoneNumber" 
                    [ Field Required (ScalarType StringType) "number" 1 []
                    , Field Required (ScalarType Int32Type) "type" 2 []
                    ] [] []
            let msg = Message "Person" 
                    [ Field Required (ScalarType StringType) "name" 1 []
                    , Field Required (ScalarType Int32Type) "age" 2 []
                    , Field Repeated (UserDefinedType "PhoneNumber") "phones" 3 []
                    ] [NestedMessage nestedMsg] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
            code `shouldContain` "data Person = Person"
            code `shouldContain` "phones :: [PhoneNumber]"
            code `shouldContain` "data PhoneNumber = PhoneNumber"
        
        it "should generate map type" $ do
            let msg = Message "Person" 
                    [ Field Required (MapType (ScalarType StringType) (ScalarType Int32Type)) "scores" 1 []
                    ] [] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
            code `shouldContain` "scores :: Map Text Int32"
        
        it "should generate repeated fields" $ do
            let msg = Message "Person" 
                    [ Field Repeated (ScalarType StringType) "tags" 1 []
                    ] [] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
            code `shouldContain` "tags :: [Text]"

-- 3. 타입 매핑 테스트

testTypeMapping :: IO ()
testTypeMapping = hspec $ do
    describe "Type Mapping" $ do
        it "should map scalar types correctly" $ do
            let scalarTypes = 
                    [ (DoubleType, "Double")
                    , (FloatType, "Float")
                    , (Int32Type, "Int32")
                    , (Int64Type, "Int64")
                    , (UInt32Type, "Word32")
                    , (UInt64Type, "Word64")
                    , (BoolType, "Bool")
                    , (StringType, "Text")
                    , (BytesType, "ByteString")
                    ]
            mapM_ (\(scalarType, expectedType) -> do
                let msg = Message "Test" [Field Required (ScalarType scalarType) "field" 1 []] [] []
                let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
                code `shouldContain` ("field :: " ++ expectedType)
                ) scalarTypes
        
        it "should handle user defined types" $ do
            let msg = Message "Person" 
                    [ Field Required (UserDefinedType "Address") "address" 1 []
                    ] [] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
            code `shouldContain` "address :: Address"

-- 4. 코드 품질 테스트

testCodeQuality :: IO ()
testCodeQuality = hspec $ do
    describe "Code Quality" $ do
        it "should generate valid Haskell syntax" $ do
            let msg = Message "Person" 
                    [ Field Required (ScalarType StringType) "name" 1 []
                    , Field Required (ScalarType Int32Type) "age" 2 []
                    ] [] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
            -- 기본적인 Haskell 문법 검사
            code `shouldContain` "data Person = Person {"
            code `shouldContain` "} deriving"
            code `shouldContain` "import Data.Text"
            code `shouldContain` "import Data.ByteString"
        
        it "should include necessary imports" $ do
            let msg = Message "Person" 
                    [ Field Required (ScalarType StringType) "name" 1 []
                    , Field Required (ScalarType Int32Type) "age" 2 []
                    ] [] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
            code `shouldContain` "import Data.Text (Text)"
            code `shouldContain` "import Data.ByteString (ByteString)"
            code `shouldContain` "import Data.Int (Int32, Int64)"
            code `shouldContain` "import Data.Word (Word32, Word64)"
            code `shouldContain` "import GHC.Generics (Generic)"

-- 5. 에러 케이스 테스트

testErrorCases :: IO ()
testErrorCases = hspec $ do
    describe "Code Generation Error Cases" $ do
        it "should handle empty message" $ do
            let msg = Message "EmptyMessage" [] [] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
            code `shouldContain` "data EmptyMessage = EmptyMessage"
            code `shouldContain` "} deriving (Show, Eq, Generic)"
        
        it "should handle message with only nested types" $ do
            let nestedMsg = Message "Nested" [Field Required (ScalarType StringType) "field" 1 []] [] []
            let msg = Message "Parent" [] [NestedMessage nestedMsg] []
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage msg]
            code `shouldContain` "data Parent = Parent"
            code `shouldContain` "data Nested = Nested"

-- 6. 성능 테스트

testPerformance :: IO ()
testPerformance = hspec $ do
    describe "Code Generation Performance" $ do
        it "should generate code for large message quickly" $ do
            let largeMessage = generateLargeMessage 100
            let code = generateCodeString $ ProtobufFile "proto3" Nothing [] [] [FileMessage largeMessage]
            code `shouldContain` "data LargeMessage = LargeMessage"
            code `shouldContain` "field1 :: Text"
            code `shouldContain` "field100 :: Text"

-- 7. 유틸리티 함수들

-- 큰 메시지 생성
generateLargeMessage :: Int -> Message
generateLargeMessage fieldCount = 
    let fields = map generateField [1..fieldCount]
    in Message "LargeMessage" fields [] []

-- 필드 생성
generateField :: Int -> Field
generateField n = Field Required (ScalarType StringType) ("field" ++ show n) n []

-- 8. 메인 테스트 함수

runAllTests :: IO ()
runAllTests = do
    putStrLn "Running Protobuf Code Generation Tests..."
    testBasicCodeGeneration
    testComplexCodeGeneration
    testTypeMapping
    testCodeQuality
    testErrorCases
    testPerformance
    putStrLn "All tests completed!"
