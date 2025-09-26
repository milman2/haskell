{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Protobuf.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

-- 1. Protobuf 기본 타입 정의

-- Protobuf 기본 스칼라 타입
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
    deriving (Show, Eq, Generic)

-- Protobuf 필드 규칙 (repeated, optional, required)
data FieldRule
    = Required
    | Optional
    | Repeated
    deriving (Show, Eq, Generic)

-- Protobuf 필드 정의
data Field = Field
    { fieldRule :: FieldRule
    , fieldType :: FieldType
    , fieldName :: Text
    , fieldNumber :: Int
    , fieldOptions :: [FieldOption]
    } deriving (Show, Eq, Generic)

-- 필드 타입 (스칼라 또는 사용자 정의 타입)
data FieldType
    = ScalarType ProtobufScalarType
    | UserDefinedType Text
    | MapType FieldType FieldType  -- Map<KeyType, ValueType>
    deriving (Show, Eq, Generic)

-- 필드 옵션
data FieldOption = FieldOption
    { optionName :: Text
    , optionValue :: OptionValue
    } deriving (Show, Eq, Generic)

-- 옵션 값
data OptionValue
    = BoolOption Bool
    | IntOption Int
    | StringOption Text
    | FloatOption Double
    deriving (Show, Eq, Generic)

-- 2. Protobuf 메시지 정의

-- Protobuf 메시지
data Message = Message
    { messageName :: Text
    , messageFields :: [Field]
    , messageNestedTypes :: [NestedType]
    , messageOptions :: [MessageOption]
    } deriving (Show, Eq, Generic)

-- 중첩된 타입 (메시지, 열거형, 서비스)
data NestedType
    = NestedMessage Message
    | NestedEnum ProtobufEnum
    | NestedService Service
    deriving (Show, Eq, Generic)

-- 메시지 옵션
data MessageOption = MessageOption
    { messageOptionName :: Text
    , messageOptionValue :: OptionValue
    } deriving (Show, Eq, Generic)

-- 3. Protobuf 열거형 정의

-- Protobuf 열거형
data ProtobufEnum = ProtobufEnum
    { enumName :: Text
    , enumValues :: [EnumValue]
    , enumOptions :: [EnumOption]
    } deriving (Show, Eq, Generic)

-- 열거형 값
data EnumValue = EnumValue
    { enumValueName :: Text
    , enumValueNumber :: Int
    , enumValueOptions :: [EnumValueOption]
    } deriving (Show, Eq, Generic)

-- 열거형 옵션
data EnumOption = EnumOption
    { enumOptionName :: Text
    , enumOptionValue :: OptionValue
    } deriving (Show, Eq, Generic)

-- 열거형 값 옵션
data EnumValueOption = EnumValueOption
    { enumValueOptionName :: Text
    , enumValueOptionValue :: OptionValue
    } deriving (Show, Eq, Generic)

-- 4. Protobuf 서비스 정의

-- Protobuf 서비스
data Service = Service
    { serviceName :: Text
    , serviceMethods :: [Method]
    , serviceOptions :: [ServiceOption]
    } deriving (Show, Eq, Generic)

-- 서비스 메서드
data Method = Method
    { methodName :: Text
    , methodInputType :: Text
    , methodOutputType :: Text
    , methodOptions :: [MethodOption]
    } deriving (Show, Eq, Generic)

-- 서비스 옵션
data ServiceOption = ServiceOption
    { serviceOptionName :: Text
    , serviceOptionValue :: OptionValue
    } deriving (Show, Eq, Generic)

-- 메서드 옵션
data MethodOption = MethodOption
    { methodOptionName :: Text
    , methodOptionValue :: OptionValue
    } deriving (Show, Eq, Generic)

-- 5. Protobuf 파일 정의

-- Protobuf 파일
data ProtobufFile = ProtobufFile
    { fileSyntax :: Text  -- "proto2" 또는 "proto3"
    , filePackage :: Maybe Text
    , fileImports :: [Text]
    , fileOptions :: [FileOption]
    , fileDefinitions :: [FileDefinition]
    } deriving (Show, Eq, Generic)

-- 파일 정의 (메시지, 열거형, 서비스)
data FileDefinition
    = FileMessage Message
    | FileEnum ProtobufEnum
    | FileService Service
    deriving (Show, Eq, Generic)

-- 파일 옵션
data FileOption = FileOption
    { fileOptionName :: Text
    , fileOptionValue :: OptionValue
    } deriving (Show, Eq, Generic)

-- 6. 파싱 에러 타입

-- 파싱 에러
data ParseError
    = SyntaxError Text
    | TypeError Text
    | ValidationError Text
    | ImportError Text
    deriving (Show, Eq, Generic)

-- 7. 유틸리티 함수들

-- 필드 번호 유효성 검사
isValidFieldNumber :: Int -> Bool
isValidFieldNumber n = n > 0 && n <= 536870911  -- 2^29 - 1

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
