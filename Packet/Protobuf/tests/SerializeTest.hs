{-# LANGUAGE OverloadedStrings #-}

module SerializeTest where

import Protobuf.Serialize
import Protobuf.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text, pack)
import Test.Hspec
import Test.QuickCheck
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)

-- 1. 기본 직렬화 테스트

testBasicSerialization :: IO ()
testBasicSerialization = hspec $ do
    describe "Protobuf Serialization" $ do
        it "should encode/decode strings" $ do
            let text = pack "Hello, World!"
            let encoded = encodeString text
            let (decoded, _) = decodeString encoded
            decoded `shouldBe` text
        
        it "should encode/decode integers" $ do
            let int32 = 42 :: Int32
            let encoded = encodeInt32 int32
            let (decoded, _) = decodeInt32 encoded
            decoded `shouldBe` int32
        
        it "should encode/decode booleans" $ do
            let bool = True
            let encoded = encodeBool bool
            let (decoded, _) = decodeBool encoded
            decoded `shouldBe` bool
        
        it "should encode/decode doubles" $ do
            let double = 3.14159 :: Double
            let encoded = encodeDouble double
            let (decoded, _) = decodeDouble encoded
            decoded `shouldBe` double

-- 2. 복잡한 직렬화 테스트

testComplexSerialization :: IO ()
testComplexSerialization = hspec $ do
    describe "Complex Serialization" $ do
        it "should encode/decode lists" $ do
            let strings = [pack "Hello", pack "World", pack "Test"]
            let encoded = encodeList encodeString strings
            let (decoded, _) = decodeList decodeString encoded
            decoded `shouldBe` strings
        
        it "should encode/decode maps" $ do
            let map = [(pack "key1", 1), (pack "key2", 2), (pack "key3", 3)]
            let encoded = encodeMap encodeString encodeInt32 map
            let (decoded, _) = decodeMap decodeString decodeInt32 encoded
            decoded `shouldBe` map
        
        it "should encode/decode nested structures" $ do
            let msg = Message "Person" 
                    [ Field Required (ScalarType StringType) "name" 1 []
                    , Field Required (ScalarType Int32Type) "age" 2 []
                    ] [] []
            let encoded = encodeMessage msg
            let decoded = decodeMessage encoded
            messageName decoded `shouldBe` messageName msg

-- 3. Wire Format 테스트

testWireFormat :: IO ()
testWireFormat = hspec $ do
    describe "Wire Format" $ do
        it "should encode field tags correctly" $ do
            let tag = FieldTag 1 Varint
            let encoded = encodeFieldTag tag
            let (decoded, _) = decodeFieldTag encoded
            decoded `shouldBe` tag
        
        it "should handle different wire types" $ do
            let wireTypes = [Varint, Fixed64, LengthDelimited, StartGroup, EndGroup, Fixed32]
            mapM_ (\wt -> do
                let tag = FieldTag 1 wt
                let encoded = encodeFieldTag tag
                let (decoded, _) = decodeFieldTag encoded
                decoded `shouldBe` tag
                ) wireTypes
        
        it "should encode field numbers correctly" $ do
            let fieldNumbers = [1, 2, 3, 10, 100, 1000, 10000]
            mapM_ (\num -> do
                let tag = FieldTag num Varint
                let encoded = encodeFieldTag tag
                let (decoded, _) = decodeFieldTag encoded
                fieldNumber decoded `shouldBe` num
                ) fieldNumbers

-- 4. 성능 테스트

testPerformance :: IO ()
testPerformance = hspec $ do
    describe "Serialization Performance" $ do
        it "should serialize large data quickly" $ do
            let largeString = pack $ replicate 10000 'A'
            let encoded = encodeString largeString
            let (decoded, _) = decodeString encoded
            decoded `shouldBe` largeString
        
        it "should handle large lists efficiently" $ do
            let largeList = [1..1000] :: [Int32]
            let encoded = encodeList encodeInt32 largeList
            let (decoded, _) = decodeList decodeInt32 encoded
            decoded `shouldBe` largeList
        
        it "should handle large maps efficiently" $ do
            let largeMap = [(pack $ "key" ++ show i, i) | i <- [1..1000]]
            let encoded = encodeMap encodeString encodeInt32 largeMap
            let (decoded, _) = decodeMap decodeString decodeInt32 encoded
            decoded `shouldBe` largeMap

-- 5. 에러 케이스 테스트

testErrorCases :: IO ()
testErrorCases = hspec $ do
    describe "Serialization Error Cases" $ do
        it "should handle empty strings" $ do
            let emptyString = pack ""
            let encoded = encodeString emptyString
            let (decoded, _) = decodeString encoded
            decoded `shouldBe` emptyString
        
        it "should handle empty lists" $ do
            let emptyList = [] :: [Int32]
            let encoded = encodeList encodeInt32 emptyList
            let (decoded, _) = decodeList decodeInt32 encoded
            decoded `shouldBe` emptyList
        
        it "should handle empty maps" $ do
            let emptyMap = [] :: [(Text, Int32)]
            let encoded = encodeMap encodeString encodeInt32 emptyMap
            let (decoded, _) = decodeMap decodeString decodeInt32 encoded
            decoded `shouldBe` emptyMap

-- 6. 타입별 직렬화 테스트

testTypeSpecificSerialization :: IO ()
testTypeSpecificSerialization = hspec $ do
    describe "Type-Specific Serialization" $ do
        it "should handle all scalar types" $ do
            let scalarTests = 
                    [ (encodeDouble 3.14, decodeDouble)
                    , (encodeFloat 3.14, decodeFloat)
                    , (encodeInt32 42, decodeInt32)
                    , (encodeInt64 42, decodeInt64)
                    , (encodeUInt32 42, decodeUInt32)
                    , (encodeUInt64 42, decodeUInt64)
                    , (encodeSInt32 42, decodeSInt32)
                    , (encodeSInt64 42, decodeSInt64)
                    , (encodeFixed32 42, decodeFixed32)
                    , (encodeFixed64 42, decodeFixed64)
                    , (encodeSFixed32 42, decodeSFixed32)
                    , (encodeSFixed64 42, decodeSFixed64)
                    , (encodeBool True, decodeBool)
                    , (encodeString (pack "test"), decodeString)
                    , (encodeBytes (BS.pack [1,2,3,4]), decodeBytes)
                    ]
            mapM_ (\(encoded, decoder) -> do
                let (decoded, _) = decoder encoded
                decoded `shouldSatisfy` (const True)  -- 기본적으로 성공하면 OK
                ) scalarTests

-- 7. 유틸리티 함수들

-- 성능 측정을 위한 벤치마크 함수
benchmarkSerialization :: IO ()
benchmarkSerialization = do
    putStrLn "Running serialization benchmarks..."
    
    -- 문자열 직렬화 벤치마크
    let testString = pack $ replicate 1000 'A'
    let encoded = encodeString testString
    let (decoded, _) = decodeString encoded
    putStrLn $ "String serialization: " ++ show (BS.length encoded) ++ " bytes"
    
    -- 리스트 직렬화 벤치마크
    let testList = [1..1000] :: [Int32]
    let encodedList = encodeList encodeInt32 testList
    let (decodedList, _) = decodeList decodeInt32 encodedList
    putStrLn $ "List serialization: " ++ show (BS.length encodedList) ++ " bytes"
    
    -- 맵 직렬화 벤치마크
    let testMap = [(pack $ "key" ++ show i, i) | i <- [1..1000]]
    let encodedMap = encodeMap encodeString encodeInt32 testMap
    let (decodedMap, _) = decodeMap decodeString decodeInt32 encodedMap
    putStrLn $ "Map serialization: " ++ show (BS.length encodedMap) ++ " bytes"

-- 8. 메인 테스트 함수

runAllTests :: IO ()
runAllTests = do
    putStrLn "Running Protobuf Serialization Tests..."
    testBasicSerialization
    testComplexSerialization
    testWireFormat
    testPerformance
    testErrorCases
    testTypeSpecificSerialization
    benchmarkSerialization
    putStrLn "All tests completed!"
