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
        nestedTypes = messageNestedTypes msg
        fieldStrings = map generateFieldCode fields
        indentedFields = map ("  " ++) fieldStrings
        nestedTypeCodes = concatMap (++ [""]) (map generateNestedTypeCode nestedTypes)
    in nestedTypeCodes ++
       [""] ++  -- 빈 줄 추가
       [unwords ["data", typeName, "=", typeName, "{"]] ++
        indentedFields ++
        ["} deriving (Show, Eq, Generic)"]

-- 중첩된 타입 코드 생성
generateNestedTypeCode :: NestedType -> [String]
generateNestedTypeCode (NestedMessage msg) = generateMessageCode msg
generateNestedTypeCode (NestedEnum enum) = generateEnumCode enum

-- 필드 코드 생성
generateFieldCode :: Field -> String
generateFieldCode field = 
    let fieldNameStr = unpack (fieldName field)
        fieldTypeStr = generateFieldTypeCode (fieldType field)
        repeatedTypeStr = if fieldRule field == Repeated 
                         then "[" ++ fieldTypeStr ++ "]"
                         else fieldTypeStr
    in unwords [fieldNameStr, "::", repeatedTypeStr, ","]

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
        " " ++ intercalate " | " valueStrings ++
        " deriving (Show, Eq, Generic)"
        ] ++
        [""]  -- 빈 줄 추가

-- 서비스 코드 생성
generateServiceCode :: Service -> [String]
generateServiceCode service = 
    let className = unpack (serviceName service)
        methods = serviceMethods service
        methodStrings = map generateMethodCode methods
        indentedMethods = map ("  " ++) methodStrings
    in [""] ++  -- 빈 줄 추가
       [unwords ["class", className, "m where"]] ++
        indentedMethods

-- 메서드 코드 생성
generateMethodCode :: Method -> String
generateMethodCode method = 
    let methodNameStr = unpack (methodName method)
        inputTypeStr = capitalizeTypeName (unpack (methodInputType method))
        outputTypeStr = capitalizeTypeName (unpack (methodOutputType method))
    in unwords [methodNameStr, "::", inputTypeStr, "->", "m", outputTypeStr]

-- 타입 이름을 대문자로 변환 (int32 -> Int32)
capitalizeTypeName :: String -> String
capitalizeTypeName "int32" = "Int32"
capitalizeTypeName "int64" = "Int64"
capitalizeTypeName "uint32" = "Word32"
capitalizeTypeName "uint64" = "Word64"
capitalizeTypeName "string" = "Text"
capitalizeTypeName "bool" = "Bool"
capitalizeTypeName "double" = "Double"
capitalizeTypeName "float" = "Float"
capitalizeTypeName name = name -- 사용자 정의 타입은 그대로

-- 2. 유틸리티 함수들

-- 문자열 연결
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs