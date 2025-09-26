{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Protobuf.Serialize where

import Protobuf.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (Binary, encode, decode)
import Data.Binary.Put (Put, putWord8, putWord32le, putWord64le, putInt32le, putInt64le, putFloatle, putDoublele)
import Data.Binary.Get (Get, getWord8, getWord32le, getWord64le, getInt32le, getInt64le, getFloatle, getDoublele)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad (replicateM)

-- 1. Protobuf Wire Format 타입

-- Wire 타입 정의
data WireType
    = Varint
    | Fixed64
    | LengthDelimited
    | StartGroup
    | EndGroup
    | Fixed32
    deriving (Show, Eq, Enum)

-- 필드 태그 (필드 번호 + Wire 타입)
data FieldTag = FieldTag
    { fieldNumber :: Int
    , wireType :: WireType
    } deriving (Show, Eq)

-- 2. 기본 인코딩 함수들

-- Varint 인코딩
encodeVarint :: Word64 -> ByteString
encodeVarint n = BS.pack $ varintBytes n
  where
    varintBytes 0 = [0]
    varintBytes n = if n < 128 then [fromIntegral n] else (fromIntegral (n .&. 127) .|. 128) : varintBytes (n `shiftR` 7)

-- Varint 디코딩
decodeVarint :: ByteString -> (Word64, ByteString)
decodeVarint bs = decodeVarint' 0 0 bs
  where
    decodeVarint' result shift bs
        | BS.null bs = (result, bs)
        | otherwise = 
            let (byte, rest) = (BS.head bs, BS.tail bs)
                hasMore = byte .&. 128 /= 0
                value = fromIntegral (byte .&. 127)
                newResult = result .|. (value `shiftL` shift)
            in if hasMore
               then decodeVarint' newResult (shift + 7) rest
               else (newResult, rest)

-- 필드 태그 인코딩
encodeFieldTag :: FieldTag -> ByteString
encodeFieldTag (FieldTag num wireType) = 
    let tag = (num `shiftL` 3) .|. fromIntegral (fromEnum wireType)
    in encodeVarint (fromIntegral tag)

-- 필드 태그 디코딩
decodeFieldTag :: ByteString -> (FieldTag, ByteString)
decodeFieldTag bs = 
    let (tag, rest) = decodeVarint bs
        fieldNumber = fromIntegral (tag `shiftR` 3)
        wireType = toEnum (fromIntegral (tag .&. 7))
    in (FieldTag fieldNumber wireType, rest)

-- 3. 스칼라 타입 인코딩/디코딩

-- Double 인코딩
encodeDouble :: Double -> ByteString
encodeDouble = BS.pack . LBS.unpack . encode

-- Double 디코딩
decodeDouble :: ByteString -> (Double, ByteString)
decodeDouble bs = 
    let (value, rest) = (LBS.unpack $ LBS.fromStrict bs, BS.drop 8 bs)
    in (decode $ LBS.pack value, rest)

-- Float 인코딩
encodeFloat :: Float -> ByteString
encodeFloat = BS.pack . LBS.unpack . encode

-- Float 디코딩
decodeFloat :: ByteString -> (Float, ByteString)
decodeFloat bs = 
    let (value, rest) = (LBS.unpack $ LBS.fromStrict bs, BS.drop 4 bs)
    in (decode $ LBS.pack value, rest)

-- Int32 인코딩 (Varint)
encodeInt32 :: Int32 -> ByteString
encodeInt32 = encodeVarint . fromIntegral

-- Int32 디코딩
decodeInt32 :: ByteString -> (Int32, ByteString)
decodeInt32 bs = 
    let (value, rest) = decodeVarint bs
    in (fromIntegral value, rest)

-- Int64 인코딩 (Varint)
encodeInt64 :: Int64 -> ByteString
encodeInt64 = encodeVarint . fromIntegral

-- Int64 디코딩
decodeInt64 :: ByteString -> (Int64, ByteString)
decodeInt64 bs = 
    let (value, rest) = decodeVarint bs
    in (fromIntegral value, rest)

-- UInt32 인코딩 (Varint)
encodeUInt32 :: Word32 -> ByteString
encodeUInt32 = encodeVarint . fromIntegral

-- UInt32 디코딩
decodeUInt32 :: ByteString -> (Word32, ByteString)
decodeUInt32 bs = 
    let (value, rest) = decodeVarint bs
    in (fromIntegral value, rest)

-- UInt64 인코딩 (Varint)
encodeUInt64 :: Word64 -> ByteString
encodeUInt64 = encodeVarint

-- UInt64 디코딩
decodeUInt64 :: ByteString -> (Word64, ByteString)
decodeUInt64 bs = decodeVarint bs

-- SInt32 인코딩 (ZigZag)
encodeSInt32 :: Int32 -> ByteString
encodeSInt32 n = encodeVarint $ fromIntegral $ zigzag32 n
  where
    zigzag32 n = if n >= 0 then fromIntegral (n `shiftL` 1) else fromIntegral ((-n) `shiftL` 1 - 1)

-- SInt32 디코딩
decodeSInt32 :: ByteString -> (Int32, ByteString)
decodeSInt32 bs = 
    let (value, rest) = decodeVarint bs
    in (unzigzag32 $ fromIntegral value, rest)
  where
    unzigzag32 n = if n `mod` 2 == 0 then fromIntegral (n `shiftR` 1) else fromIntegral (-((n + 1) `shiftR` 1))

-- SInt64 인코딩 (ZigZag)
encodeSInt64 :: Int64 -> ByteString
encodeSInt64 n = encodeVarint $ fromIntegral $ zigzag64 n
  where
    zigzag64 n = if n >= 0 then fromIntegral (n `shiftL` 1) else fromIntegral ((-n) `shiftL` 1 - 1)

-- SInt64 디코딩
decodeSInt64 :: ByteString -> (Int64, ByteString)
decodeSInt64 bs = 
    let (value, rest) = decodeVarint bs
    in (unzigzag64 $ fromIntegral value, rest)
  where
    unzigzag64 n = if n `mod` 2 == 0 then fromIntegral (n `shiftR` 1) else fromIntegral (-((n + 1) `shiftR` 1))

-- Fixed32 인코딩
encodeFixed32 :: Word32 -> ByteString
encodeFixed32 = BS.pack . LBS.unpack . encode

-- Fixed32 디코딩
decodeFixed32 :: ByteString -> (Word32, ByteString)
decodeFixed32 bs = 
    let (value, rest) = (LBS.unpack $ LBS.fromStrict bs, BS.drop 4 bs)
    in (decode $ LBS.pack value, rest)

-- Fixed64 인코딩
encodeFixed64 :: Word64 -> ByteString
encodeFixed64 = BS.pack . LBS.unpack . encode

-- Fixed64 디코딩
decodeFixed64 :: ByteString -> (Word64, ByteString)
decodeFixed64 bs = 
    let (value, rest) = (LBS.unpack $ LBS.fromStrict bs, BS.drop 8 bs)
    in (decode $ LBS.pack value, rest)

-- SFixed32 인코딩
encodeSFixed32 :: Int32 -> ByteString
encodeSFixed32 = BS.pack . LBS.unpack . encode

-- SFixed32 디코딩
decodeSFixed32 :: ByteString -> (Int32, ByteString)
decodeSFixed32 bs = 
    let (value, rest) = (LBS.unpack $ LBS.fromStrict bs, BS.drop 4 bs)
    in (decode $ LBS.pack value, rest)

-- SFixed64 인코딩
encodeSFixed64 :: Int64 -> ByteString
encodeSFixed64 = BS.pack . LBS.unpack . encode

-- SFixed64 디코딩
decodeSFixed64 :: ByteString -> (Int64, ByteString)
decodeSFixed64 bs = 
    let (value, rest) = (LBS.unpack $ LBS.fromStrict bs, BS.drop 8 bs)
    in (decode $ LBS.pack value, rest)

-- Bool 인코딩
encodeBool :: Bool -> ByteString
encodeBool True = encodeVarint 1
encodeBool False = encodeVarint 0

-- Bool 디코딩
decodeBool :: ByteString -> (Bool, ByteString)
decodeBool bs = 
    let (value, rest) = decodeVarint bs
    in (value /= 0, rest)

-- String 인코딩
encodeString :: Text -> ByteString
encodeString text = 
    let encoded = TE.encodeUtf8 text
        length = BS.length encoded
    in encodeVarint (fromIntegral length) `BS.append` encoded

-- String 디코딩
decodeString :: ByteString -> (Text, ByteString)
decodeString bs = 
    let (length, rest1) = decodeVarint bs
        (text, rest2) = BS.splitAt (fromIntegral length) rest1
    in (TE.decodeUtf8 text, rest2)

-- Bytes 인코딩
encodeBytes :: ByteString -> ByteString
encodeBytes bytes = 
    let length = BS.length bytes
    in encodeVarint (fromIntegral length) `BS.append` bytes

-- Bytes 디코딩
decodeBytes :: ByteString -> (ByteString, ByteString)
decodeBytes bs = 
    let (length, rest1) = decodeVarint bs
        (bytes, rest2) = BS.splitAt (fromIntegral length) rest1
    in (bytes, rest2)

-- 4. 복합 타입 인코딩/디코딩

-- 리스트 인코딩
encodeList :: (a -> ByteString) -> [a] -> ByteString
encodeList encoder items = 
    let encodedItems = map encoder items
        totalLength = sum $ map BS.length encodedItems
    in encodeVarint (fromIntegral totalLength) `BS.append` BS.concat encodedItems

-- 리스트 디코딩
decodeList :: (ByteString -> (a, ByteString)) -> ByteString -> ([a], ByteString)
decodeList decoder bs = 
    let (length, rest1) = decodeVarint bs
        (items, rest2) = BS.splitAt (fromIntegral length) rest1
    in decodeListItems decoder items rest2
  where
    decodeListItems decoder bs rest
        | BS.null bs = ([], rest)
        | otherwise = 
            let (item, newBs) = decoder bs
                (items, finalRest) = decodeListItems decoder newBs rest
            in (item : items, finalRest)

-- 맵 인코딩
encodeMap :: (k -> ByteString) -> (v -> ByteString) -> Map k v -> ByteString
encodeMap keyEncoder valueEncoder map = 
    let entries = M.toList map
        encodedEntries = map (\(k, v) -> keyEncoder k `BS.append` valueEncoder v) entries
        totalLength = sum $ map BS.length encodedEntries
    in encodeVarint (fromIntegral totalLength) `BS.append` BS.concat encodedEntries

-- 맵 디코딩
decodeMap :: (ByteString -> (k, ByteString)) -> (ByteString -> (v, ByteString)) -> ByteString -> (Map k v, ByteString)
decodeMap keyDecoder valueDecoder bs = 
    let (length, rest1) = decodeVarint bs
        (entries, rest2) = BS.splitAt (fromIntegral length) rest1
    in decodeMapEntries keyDecoder valueDecoder entries rest2
  where
    decodeMapEntries keyDecoder valueDecoder bs rest
        | BS.null bs = (M.empty, rest)
        | otherwise = 
            let (key, bs1) = keyDecoder bs
                (value, bs2) = valueDecoder bs1
                (map, finalRest) = decodeMapEntries keyDecoder valueDecoder bs2 rest
            in (M.insert key value map, finalRest)

-- 5. 필드 인코딩/디코딩

-- 필드 인코딩
encodeField :: Int -> ByteString -> ByteString
encodeField fieldNumber value = 
    let tag = encodeFieldTag (FieldTag fieldNumber LengthDelimited)
        length = encodeVarint (fromIntegral $ BS.length value)
    in tag `BS.append` length `BS.append` value

-- 필드 디코딩
decodeField :: ByteString -> (Int, ByteString, ByteString)
decodeField bs = 
    let (tag, rest1) = decodeFieldTag bs
        (length, rest2) = decodeVarint rest1
        (value, rest3) = BS.splitAt (fromIntegral length) rest2
    in (Protobuf.Serialize.fieldNumber tag, value, rest3)

-- 6. 메시지 인코딩/디코딩

-- 메시지 인코딩
encodeMessage :: Message -> ByteString
encodeMessage msg = 
    let fields = messageFields msg
        encodedFields = map encodeFieldValue fields
    in BS.concat encodedFields

-- 필드 값 인코딩
encodeFieldValue :: Field -> ByteString
encodeFieldValue field = 
    let fieldNumber = fieldNumber field
        fieldType = fieldType field
        value = encodeFieldType fieldType
    in encodeField fieldNumber value

-- 필드 타입 인코딩
encodeFieldType :: FieldType -> ByteString
encodeFieldType (ScalarType scalarType) = encodeScalarType scalarType
encodeFieldType (UserDefinedType _) = BS.empty  -- TODO: 사용자 정의 타입 처리
encodeFieldType (MapType _ _) = BS.empty  -- TODO: 맵 타입 처리

-- 스칼라 타입 인코딩
encodeScalarType :: ProtobufScalarType -> ByteString
encodeScalarType DoubleType = encodeDouble 0.0
encodeScalarType FloatType = Protobuf.Serialize.encodeFloat 0.0
encodeScalarType Int32Type = encodeInt32 0
encodeScalarType Int64Type = encodeInt64 0
encodeScalarType UInt32Type = encodeUInt32 0
encodeScalarType UInt64Type = encodeUInt64 0
encodeScalarType SInt32Type = encodeSInt32 0
encodeScalarType SInt64Type = encodeSInt64 0
encodeScalarType Fixed32Type = encodeFixed32 0
encodeScalarType Fixed64Type = encodeFixed64 0
encodeScalarType SFixed32Type = encodeSFixed32 0
encodeScalarType SFixed64Type = encodeSFixed64 0
encodeScalarType BoolType = encodeBool False
encodeScalarType StringType = encodeString ""
encodeScalarType BytesType = encodeBytes BS.empty

-- 메시지 디코딩
decodeMessage :: ByteString -> Message
decodeMessage bs = 
    let fields = decodeMessageFields bs
    in Message "" fields [] []  -- TODO: 실제 메시지 구조로 디코딩

-- 메시지 필드 디코딩
decodeMessageFields :: ByteString -> [Field]
decodeMessageFields bs
    | BS.null bs = []
    | otherwise = 
        let (fieldNumber, value, rest) = decodeField bs
            field = Field Required (ScalarType StringType) "" fieldNumber []
            (fields, finalRest) = decodeMessageFields rest
        in field : fields
