{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import Protobuf.Parser
import Protobuf.Types
import Data.Text (Text, pack)
import Test.Hspec
import Test.QuickCheck

-- 1. 기본 파서 테스트

testBasicParsing :: IO ()
testBasicParsing = hspec $ do
    describe "Protobuf Parser" $ do
        it "should parse simple message" $ do
            let input = "message Person { string name = 1; int32 age = 2; }"
            case parseProtobufFile (pack input) of
                Left _ -> fail "Failed to parse simple message"
                Right file -> do
                    length (fileDefinitions file) `shouldBe` 1
                    case head (fileDefinitions file) of
                        FileMessage msg -> do
                            messageName msg `shouldBe` "Person"
                            length (messageFields msg) `shouldBe` 2
                        _ -> fail "Expected message definition"
        
        it "should parse enum" $ do
            let input = "enum Color { RED = 0; GREEN = 1; BLUE = 2; }"
            case parseProtobufFile (pack input) of
                Left _ -> fail "Failed to parse enum"
                Right file -> do
                    length (fileDefinitions file) `shouldBe` 1
                    case head (fileDefinitions file) of
                        FileEnum enum -> do
                            enumName enum `shouldBe` "Color"
                            length (enumValues enum) `shouldBe` 3
                        _ -> fail "Expected enum definition"
        
        it "should parse service" $ do
            let input = "service UserService { rpc GetUser(int32) returns (User); }"
            case parseProtobufFile (pack input) of
                Left _ -> fail "Failed to parse service"
                Right file -> do
                    length (fileDefinitions file) `shouldBe` 1
                    case head (fileDefinitions file) of
                        FileService service -> do
                            serviceName service `shouldBe` "UserService"
                            length (serviceMethods service) `shouldBe` 1
                        _ -> fail "Expected service definition"

-- 2. 복잡한 파서 테스트

testComplexParsing :: IO ()
testComplexParsing = hspec $ do
    describe "Complex Protobuf Parsing" $ do
        it "should parse nested message" $ do
            let input = unlines
                    [ "message Person {"
                    , "  string name = 1;"
                    , "  message PhoneNumber {"
                    , "    string number = 1;"
                    , "    PhoneType type = 2;"
                    , "  }"
                    , "  repeated PhoneNumber phones = 3;"
                    , "}"
                    ]
            case parseProtobufFile (pack input) of
                Left _ -> fail "Failed to parse nested message"
                Right file -> do
                    length (fileDefinitions file) `shouldBe` 1
                    case head (fileDefinitions file) of
                        FileMessage msg -> do
                            messageName msg `shouldBe` "Person"
                            length (messageNestedTypes msg) `shouldBe` 1
                        _ -> fail "Expected message definition"
        
        it "should parse map type" $ do
            let input = "message Person { map<string, int32> scores = 1; }"
            case parseProtobufFile (pack input) of
                Left _ -> fail "Failed to parse map type"
                Right file -> do
                    length (fileDefinitions file) `shouldBe` 1
                    case head (fileDefinitions file) of
                        FileMessage msg -> do
                            messageName msg `shouldBe` "Person"
                            length (messageFields msg) `shouldBe` 1
                            let field = head (messageFields msg)
                            case fieldType field of
                                MapType keyType valueType -> do
                                    keyType `shouldBe` ScalarType StringType
                                    valueType `shouldBe` ScalarType Int32Type
                                _ -> fail "Expected map type"
                        _ -> fail "Expected message definition"

-- 3. 에러 케이스 테스트

testErrorCases :: IO ()
testErrorCases = hspec $ do
    describe "Parser Error Cases" $ do
        it "should fail on invalid syntax" $ do
            let input = "invalid syntax"
            case parseProtobufFile (pack input) of
                Left _ -> return ()  -- 예상된 에러
                Right _ -> fail "Should have failed on invalid syntax"
        
        it "should fail on missing semicolon" $ do
            let input = "message Person { string name = 1 }"
            case parseProtobufFile (pack input) of
                Left _ -> return ()  -- 예상된 에러
                Right _ -> fail "Should have failed on missing semicolon"
        
        it "should fail on invalid field number" $ do
            let input = "message Person { string name = 0; }"
            case parseProtobufFile (pack input) of
                Left _ -> return ()  -- 예상된 에러
                Right _ -> fail "Should have failed on invalid field number"

-- 4. 성능 테스트

testPerformance :: IO ()
testPerformance = hspec $ do
    describe "Parser Performance" $ do
        it "should parse large message quickly" $ do
            let largeMessage = generateLargeMessage 100
            case parseProtobufFile (pack largeMessage) of
                Left _ -> fail "Failed to parse large message"
                Right file -> do
                    length (fileDefinitions file) `shouldBe` 1
                    case head (fileDefinitions file) of
                        FileMessage msg -> do
                            messageName msg `shouldBe` "LargeMessage"
                            length (messageFields msg) `shouldBe` 100
                        _ -> fail "Expected message definition"

-- 5. 유틸리티 함수들

-- 큰 메시지 생성
generateLargeMessage :: Int -> String
generateLargeMessage fieldCount = 
    let fields = map generateField [1..fieldCount]
        fieldStrings = unlines fields
    in "message LargeMessage {\n" ++ fieldStrings ++ "}"

-- 필드 생성
generateField :: Int -> String
generateField n = "  string field" ++ show n ++ " = " ++ show n ++ ";"

-- 6. 메인 테스트 함수

runAllTests :: IO ()
runAllTests = do
    putStrLn "Running Protobuf Parser Tests..."
    testBasicParsing
    testComplexParsing
    testErrorCases
    testPerformance
    putStrLn "All tests completed!"
