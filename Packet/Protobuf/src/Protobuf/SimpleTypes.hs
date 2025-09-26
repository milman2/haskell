{-# LANGUAGE OverloadedStrings #-}

module Protobuf.SimpleTypes where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

-- 1. 기본 Protobuf 타입들

-- Protobuf 스칼라 타입
data ProtobufScalarType
    = DoubleType
    | FloatType
    | Int32Type
    | Int64Type
    | UInt32Type
    | UInt64Type
    | SInt32Type
    | SInt64Type
    | Fixed32Type
    | Fixed64Type
    | SFixed32Type
    | SFixed64Type
    | BoolType
    | StringType
    | BytesType
    deriving (Show, Eq)

-- 필드 규칙
data FieldRule
    = Required
    | Optional
    | Repeated
    deriving (Show, Eq)

-- 필드 타입
data FieldType
    = ScalarType ProtobufScalarType
    | UserDefinedType Text
    | MapType ProtobufScalarType ProtobufScalarType
    deriving (Show, Eq)

-- 필드 정의
data Field = Field
    { fieldRule :: FieldRule
    , fieldType :: FieldType
    , fieldName :: Text
    , fieldNumber :: Int
    } deriving (Show, Eq)

-- 중첩된 타입
data NestedType
    = NestedMessage Message
    | NestedEnum ProtobufEnum
    deriving (Show, Eq)

-- 메시지 정의
data Message = Message
    { messageName :: Text
    , messageFields :: [Field]
    , messageNestedTypes :: [NestedType]
    } deriving (Show, Eq)

-- 열거형 정의
data ProtobufEnum = ProtobufEnum
    { enumName :: Text
    , enumValues :: [EnumValue]
    } deriving (Show, Eq)

-- 열거형 값
data EnumValue = EnumValue
    { enumValueName :: Text
    , enumValueNumber :: Int
    } deriving (Show, Eq)

-- 서비스 정의
data Service = Service
    { serviceName :: Text
    , serviceMethods :: [Method]
    } deriving (Show, Eq)

-- 메서드 정의
data Method = Method
    { methodName :: Text
    , methodInputType :: Text
    , methodOutputType :: Text
    } deriving (Show, Eq)

-- 파일 정의
data ProtobufFile = ProtobufFile
    { fileSyntax :: Text
    , filePackage :: Maybe Text
    , fileDefinitions :: [FileDefinition]
    } deriving (Show, Eq)

-- 파일 정의 타입
data FileDefinition
    = FileMessage Message
    | FileEnum ProtobufEnum
    | FileService Service
    deriving (Show, Eq)

-- 2. 유틸리티 함수들

-- 필드 번호 유효성 검사
isValidFieldNumber :: Int -> Bool
isValidFieldNumber n = n > 0 && n <= 536870911

-- 필드 이름 유효성 검사
isValidFieldName :: Text -> Bool
isValidFieldName name = not (T.null name) && T.head name `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

-- 메시지 이름 유효성 검사
isValidMessageName :: Text -> Bool
isValidMessageName name = not (T.null name) && T.head name `elem` ['A'..'Z']

-- 열거형 이름 유효성 검사
isValidEnumName :: Text -> Bool
isValidEnumName name = not (T.null name) && T.head name `elem` ['A'..'Z']

-- 서비스 이름 유효성 검사
isValidServiceName :: Text -> Bool
isValidServiceName name = not (T.null name) && T.head name `elem` ['A'..'Z']
