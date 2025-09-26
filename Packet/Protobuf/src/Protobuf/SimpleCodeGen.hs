{-# LANGUAGE OverloadedStrings #-}

module Protobuf.SimpleCodeGen where

import Protobuf.SimpleTypes
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

-- 1. Haskell 코드 생성

-- 전체 파일에서 Haskell 코드 생성
generateHaskellCode :: ProtobufFile -> String
generateHaskellCode file = 
    let imports = generateImports
        definitions = concatMap generateDefinitionCode (fileDefinitions file)
    in unlines $ imports ++ definitions

-- 임포트 생성
generateImports :: [String]
generateImports = 
    ["import Data.Text (Text)"
    ,"import Data.ByteString (ByteString)"
    ,"import Data.Int (Int32, Int64)"
    ,"import Data.Word (Word32, Word64)"
    ,"import GHC.Generics (Generic)"
    ,""
    ]

-- 정의에서 코드 생성
generateDefinitionCode :: FileDefinition -> [String]
generateDefinitionCode (FileMessage msg) = generateMessageCode msg
generateDefinitionCode (FileEnum enum) = generateEnumCode enum
generateDefinitionCode (FileService service) = generateServiceCode service

-- 메시지 코드 생성
generateMessageCode :: Message -> [String]
generateMessageCode msg = 
    let typeName = unpack (messageName msg)
        fields = messageFields msg
        fieldStrings = map generateFieldCode fields
    in [unwords ["data", typeName, "=", typeName, "{"] ++
        map ("  " ++) fieldStrings ++
        ["} deriving (Show, Eq, Generic)"]
        ]

-- 필드 코드 생성
generateFieldCode :: Field -> String
generateFieldCode field = 
    let fieldName = unpack (fieldName field)
        fieldType = generateFieldTypeCode (fieldType field)
    in unwords [fieldName, "::", fieldType, ","]

-- 필드 타입 코드 생성
generateFieldTypeCode :: FieldType -> String
generateFieldTypeCode (ScalarType scalarType) = generateScalarTypeCode scalarType
generateFieldTypeCode (UserDefinedType name) = unpack name
generateFieldTypeCode (MapType keyType valueType) = 
    "Map " ++ generateFieldTypeCode keyType ++ " " ++ generateFieldTypeCode valueType

-- 스칼라 타입 코드 생성
generateScalarTypeCode :: ProtobufScalarType -> String
generateScalarTypeCode DoubleType = "Double"
generateScalarTypeCode FloatType = "Float"
generateScalarTypeCode Int32Type = "Int32"
generateScalarTypeCode Int64Type = "Int64"
generateScalarTypeCode UInt32Type = "Word32"
generateScalarTypeCode UInt64Type = "Word64"
generateScalarTypeCode SInt32Type = "Int32"
generateScalarTypeCode SInt64Type = "Int64"
generateScalarTypeCode Fixed32Type = "Word32"
generateScalarTypeCode Fixed64Type = "Word64"
generateScalarTypeCode SFixed32Type = "Int32"
generateScalarTypeCode SFixed64Type = "Int64"
generateScalarTypeCode BoolType = "Bool"
generateScalarTypeCode StringType = "Text"
generateScalarTypeCode BytesType = "ByteString"

-- 열거형 코드 생성
generateEnumCode :: ProtobufEnum -> [String]
generateEnumCode enum = 
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = map (unpack . enumValueName) values
    in [unwords ["data", typeName, "="] ++
        intercalate " | " valueStrings ++
        ["deriving (Show, Eq, Generic)"]
        ]

-- 서비스 코드 생성
generateServiceCode :: Service -> [String]
generateServiceCode service = 
    let className = unpack (serviceName service)
        methods = serviceMethods service
        methodStrings = map generateMethodCode methods
    in [unwords ["class", className, "m where"] ++
        map ("  " ++) methodStrings
        ]

-- 메서드 코드 생성
generateMethodCode :: Method -> String
generateMethodCode method = 
    let methodName = unpack (methodName method)
        inputType = unpack (methodInputType method)
        outputType = unpack (methodOutputType method)
    in unwords [methodName, "::", inputType, "->", "m", outputType]

-- 2. 유틸리티 함수들

-- 문자열 연결
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs