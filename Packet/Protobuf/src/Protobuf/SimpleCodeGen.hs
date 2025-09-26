{-# LANGUAGE OverloadedStrings #-}

module Protobuf.SimpleCodeGen where

import Protobuf.SimpleTypes
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Maybe (isJust)
import Data.List (intercalate)

-- 1. Haskell 코드 생성

-- 코드 생성기 타입
type CodeGenerator = ProtobufFile -> String

-- 지원되는 코드 생성기들
codeGenerators :: [(String, CodeGenerator)]
codeGenerators =
    [ ("haskell", generateHaskellCode)
    , ("cpp", generateCppCode)
    , ("csharp", generateCSharpCode)
    , ("python", generatePythonCode)
    ]

-- 전체 파일에서 코드 생성 (언어별)
generateCode :: String -> ProtobufFile -> String
generateCode langName file = 
    case lookup langName codeGenerators of
        Just generator -> generator file
        Nothing -> generateHaskellCode file -- 기본값

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
    "Map " ++ generateScalarTypeCode keyType ++ " " ++ generateScalarTypeCode valueType

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
        ]

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

-- 2. C++ 코드 생성

-- 전체 파일에서 C++ 코드 생성
generateCppCode :: ProtobufFile -> String
generateCppCode file =
    let includes = generateCppIncludes
        namespace = case filePackage file of
            Just pkg -> ["namespace " ++ unpack pkg ++ " {", ""]
            Nothing -> []
        definitions = concatMap generateCppDefinitionCode (fileDefinitions file)
        namespaceEnd = case filePackage file of
            Just _ -> ["", "} // namespace"]
            Nothing -> []
    in unlines $ includes ++ namespace ++ definitions ++ namespaceEnd

-- C++ 헤더 포함
generateCppIncludes :: [String]
generateCppIncludes =
    ["#pragma once"
    ,"#include <string>"
    ,"#include <vector>"
    ,"#include <map>"
    ,"#include <cstdint>"
    ,""
    ]

-- C++ 정의에서 코드 생성
generateCppDefinitionCode :: FileDefinition -> [String]
generateCppDefinitionCode (FileMessage msg) = generateCppMessageCode msg
generateCppDefinitionCode (FileEnum enum) = generateCppEnumCode enum
generateCppDefinitionCode (FileService service) = generateCppServiceCode service

-- C++ 메시지 코드 생성
generateCppMessageCode :: Message -> [String]
generateCppMessageCode msg =
    let typeName = unpack (messageName msg)
        fields = messageFields msg
        nestedTypes = messageNestedTypes msg
        fieldStrings = map generateCppFieldCode fields
        nestedTypeCodes = concatMap generateCppNestedTypeCode nestedTypes
    in nestedTypeCodes ++
       [unwords ["struct", typeName, "{"]] ++
        fieldStrings ++
        ["};", ""]

-- C++ 중첩된 타입 코드 생성
generateCppNestedTypeCode :: NestedType -> [String]
generateCppNestedTypeCode (NestedMessage msg) = generateCppMessageCode msg
generateCppNestedTypeCode (NestedEnum enum) = generateCppEnumCode enum

-- C++ 필드 코드 생성
generateCppFieldCode :: Field -> String
generateCppFieldCode field =
    let fieldNameStr = unpack (fieldName field)
        fieldTypeStr = generateCppFieldTypeCode (fieldType field)
        repeatedTypeStr = if fieldRule field == Repeated
                         then "std::vector<" ++ fieldTypeStr ++ ">"
                         else fieldTypeStr
    in "  " ++ repeatedTypeStr ++ " " ++ fieldNameStr ++ ";"

-- C++ 필드 타입 코드 생성
generateCppFieldTypeCode :: FieldType -> String
generateCppFieldTypeCode (ScalarType scalarType) = generateCppScalarTypeCode scalarType
generateCppFieldTypeCode (UserDefinedType name) = unpack name
generateCppFieldTypeCode (MapType keyType valueType) =
    "std::map<" ++ generateCppScalarTypeCode keyType ++ ", " ++ generateCppScalarTypeCode valueType ++ ">"

-- C++ 스칼라 타입 코드를 C++ 타입 문자열로 변환
generateCppScalarTypeCode :: ProtobufScalarType -> String
generateCppScalarTypeCode DoubleType = "double"
generateCppScalarTypeCode FloatType = "float"
generateCppScalarTypeCode Int32Type = "int32_t"
generateCppScalarTypeCode Int64Type = "int64_t"
generateCppScalarTypeCode UInt32Type = "uint32_t"
generateCppScalarTypeCode UInt64Type = "uint64_t"
generateCppScalarTypeCode SInt32Type = "int32_t"
generateCppScalarTypeCode SInt64Type = "int64_t"
generateCppScalarTypeCode Fixed32Type = "uint32_t"
generateCppScalarTypeCode Fixed64Type = "uint64_t"
generateCppScalarTypeCode SFixed32Type = "int32_t"
generateCppScalarTypeCode SFixed64Type = "int64_t"
generateCppScalarTypeCode BoolType = "bool"
generateCppScalarTypeCode StringType = "std::string"
generateCppScalarTypeCode BytesType = "std::string"

-- C++ 열거형 코드 생성
generateCppEnumCode :: ProtobufEnum -> [String]
generateCppEnumCode enum =
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = map generateCppEnumValueCode values
    in [unwords ["enum class", typeName, "{"]] ++
        map ("  " ++) valueStrings ++
        ["};", ""]

-- C++ 열거형 값 코드 생성
generateCppEnumValueCode :: EnumValue -> String
generateCppEnumValueCode enumValue =
    let name = unpack (enumValueName enumValue)
        number = enumValueNumber enumValue
    in name ++ " = " ++ show number ++ ","

-- C++ 서비스 코드 생성
generateCppServiceCode :: Service -> [String]
generateCppServiceCode service =
    let className = unpack (serviceName service)
        methods = serviceMethods service
        methodStrings = map generateCppMethodCode methods
    in [unwords ["class", className, "{"]] ++
        ["public:"] ++
        map ("  " ++) methodStrings ++
        ["};", ""]

-- C++ 메서드 코드 생성
generateCppMethodCode :: Method -> String
generateCppMethodCode method =
    let methodNameStr = unpack (methodName method)
        inputTypeStr = capitalizeCppTypeName (unpack (methodInputType method))
        outputTypeStr = capitalizeCppTypeName (unpack (methodOutputType method))
    in unwords ["virtual", outputTypeStr, methodNameStr ++ "(" ++ inputTypeStr ++ " input) = 0;"]

-- C++ 타입 이름을 대문자로 변환
capitalizeCppTypeName :: String -> String
capitalizeCppTypeName "int32" = "int32_t"
capitalizeCppTypeName "int64" = "int64_t"
capitalizeCppTypeName "uint32" = "uint32_t"
capitalizeCppTypeName "uint64" = "uint64_t"
capitalizeCppTypeName "string" = "std::string"
capitalizeCppTypeName "bool" = "bool"
capitalizeCppTypeName "double" = "double"
capitalizeCppTypeName "float" = "float"
capitalizeCppTypeName name = name -- 사용자 정의 타입은 그대로

-- 3. C# 코드 생성

-- 전체 파일에서 C# 코드 생성
generateCSharpCode :: ProtobufFile -> String
generateCSharpCode file =
    let using = generateCSharpUsing
        namespace = case filePackage file of
            Just pkg -> ["namespace " ++ unpack pkg, "{"]
            Nothing -> []
        definitions = concatMap generateCSharpDefinitionCode (fileDefinitions file)
        namespaceEnd = case filePackage file of
            Just _ -> ["}"]
            Nothing -> []
    in unlines $ using ++ namespace ++ definitions ++ namespaceEnd

-- C# using 문
generateCSharpUsing :: [String]
generateCSharpUsing =
    ["using System;"
    ,"using System.Collections.Generic;"
    ,""
    ]

-- C# 정의에서 코드 생성
generateCSharpDefinitionCode :: FileDefinition -> [String]
generateCSharpDefinitionCode (FileMessage msg) = generateCSharpMessageCode msg
generateCSharpDefinitionCode (FileEnum enum) = generateCSharpEnumCode enum
generateCSharpDefinitionCode (FileService service) = generateCSharpServiceCode service

-- C# 메시지 코드 생성
generateCSharpMessageCode :: Message -> [String]
generateCSharpMessageCode msg =
    let typeName = unpack (messageName msg)
        fields = messageFields msg
        nestedTypes = messageNestedTypes msg
        fieldStrings = map generateCSharpFieldCode fields
        nestedTypeCodes = concatMap generateCSharpNestedTypeCode nestedTypes
    in nestedTypeCodes ++
       [unwords ["public class", typeName]] ++
       ["{"] ++
        fieldStrings ++
        ["}", ""]

-- C# 중첩된 타입 코드 생성
generateCSharpNestedTypeCode :: NestedType -> [String]
generateCSharpNestedTypeCode (NestedMessage msg) = generateCSharpMessageCode msg
generateCSharpNestedTypeCode (NestedEnum enum) = generateCSharpEnumCode enum

-- C# 필드 코드 생성
generateCSharpFieldCode :: Field -> String
generateCSharpFieldCode field =
    let fieldNameStr = unpack (fieldName field)
        fieldTypeStr = generateCSharpFieldTypeCode (fieldType field)
        repeatedTypeStr = if fieldRule field == Repeated
                         then "List<" ++ fieldTypeStr ++ ">"
                         else fieldTypeStr
    in "  public " ++ repeatedTypeStr ++ " " ++ fieldNameStr ++ " { get; set; }"

-- C# 필드 타입 코드 생성
generateCSharpFieldTypeCode :: FieldType -> String
generateCSharpFieldTypeCode (ScalarType scalarType) = generateCSharpScalarTypeCode scalarType
generateCSharpFieldTypeCode (UserDefinedType name) = unpack name
generateCSharpFieldTypeCode (MapType keyType valueType) =
    "Dictionary<" ++ generateCSharpScalarTypeCode keyType ++ ", " ++ generateCSharpScalarTypeCode valueType ++ ">"

-- C# 스칼라 타입 코드를 C# 타입 문자열로 변환
generateCSharpScalarTypeCode :: ProtobufScalarType -> String
generateCSharpScalarTypeCode DoubleType = "double"
generateCSharpScalarTypeCode FloatType = "float"
generateCSharpScalarTypeCode Int32Type = "int"
generateCSharpScalarTypeCode Int64Type = "long"
generateCSharpScalarTypeCode UInt32Type = "uint"
generateCSharpScalarTypeCode UInt64Type = "ulong"
generateCSharpScalarTypeCode SInt32Type = "int"
generateCSharpScalarTypeCode SInt64Type = "long"
generateCSharpScalarTypeCode Fixed32Type = "uint"
generateCSharpScalarTypeCode Fixed64Type = "ulong"
generateCSharpScalarTypeCode SFixed32Type = "int"
generateCSharpScalarTypeCode SFixed64Type = "long"
generateCSharpScalarTypeCode BoolType = "bool"
generateCSharpScalarTypeCode StringType = "string"
generateCSharpScalarTypeCode BytesType = "byte[]"

-- C# 열거형 코드 생성
generateCSharpEnumCode :: ProtobufEnum -> [String]
generateCSharpEnumCode enum =
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = map generateCSharpEnumValueCode values
    in [unwords ["public enum", typeName]] ++
        ["{"] ++
        map ("  " ++) valueStrings ++
        ["}", ""]

-- C# 열거형 값 코드 생성
generateCSharpEnumValueCode :: EnumValue -> String
generateCSharpEnumValueCode enumValue =
    let name = unpack (enumValueName enumValue)
        number = enumValueNumber enumValue
    in name ++ " = " ++ show number ++ ","

-- C# 서비스 코드 생성
generateCSharpServiceCode :: Service -> [String]
generateCSharpServiceCode service =
    let className = unpack (serviceName service)
        methods = serviceMethods service
        methodStrings = map generateCSharpMethodCode methods
    in [unwords ["public interface", className]] ++
        ["{"] ++
        map ("  " ++) methodStrings ++
        ["}", ""]

-- C# 메서드 코드 생성
generateCSharpMethodCode :: Method -> String
generateCSharpMethodCode method =
    let methodNameStr = unpack (methodName method)
        inputTypeStr = capitalizeCSharpTypeName (unpack (methodInputType method))
        outputTypeStr = capitalizeCSharpTypeName (unpack (methodOutputType method))
    in unwords [outputTypeStr, methodNameStr ++ "(" ++ inputTypeStr ++ " input);"]

-- C# 타입 이름을 대문자로 변환
capitalizeCSharpTypeName :: String -> String
capitalizeCSharpTypeName "int32" = "int"
capitalizeCSharpTypeName "int64" = "long"
capitalizeCSharpTypeName "uint32" = "uint"
capitalizeCSharpTypeName "uint64" = "ulong"
capitalizeCSharpTypeName "string" = "string"
capitalizeCSharpTypeName "bool" = "bool"
capitalizeCSharpTypeName "double" = "double"
capitalizeCSharpTypeName "float" = "float"
capitalizeCSharpTypeName name = name -- 사용자 정의 타입은 그대로

-- 4. Python 코드 생성

-- 전체 파일에서 Python 코드 생성
generatePythonCode :: ProtobufFile -> String
generatePythonCode file =
    let imports = generatePythonImports
        namespace = case filePackage file of
            Just pkg -> ["# Package: " ++ unpack pkg, ""]
            Nothing -> []
        definitions = concatMap generatePythonDefinitionCode (fileDefinitions file)
    in unlines $ imports ++ namespace ++ definitions

-- Python import 문
generatePythonImports :: [String]
generatePythonImports =
    ["from dataclasses import dataclass"
    ,"from typing import List, Dict, Optional"
    ,"from enum import Enum"
    ,""
    ]

-- Python 정의에서 코드 생성
generatePythonDefinitionCode :: FileDefinition -> [String]
generatePythonDefinitionCode (FileMessage msg) = generatePythonMessageCode msg
generatePythonDefinitionCode (FileEnum enum) = generatePythonEnumCode enum
generatePythonDefinitionCode (FileService service) = generatePythonServiceCode service

-- Python 메시지 코드 생성
generatePythonMessageCode :: Message -> [String]
generatePythonMessageCode msg =
    let typeName = unpack (messageName msg)
        fields = messageFields msg
        nestedTypes = messageNestedTypes msg
        fieldStrings = map generatePythonFieldCode fields
        nestedTypeCodes = concatMap generatePythonNestedTypeCode nestedTypes
    in nestedTypeCodes ++
       [unwords ["@dataclass", "class", typeName ++ ":"]] ++
        fieldStrings ++
        [""]

-- Python 중첩된 타입 코드 생성
generatePythonNestedTypeCode :: NestedType -> [String]
generatePythonNestedTypeCode (NestedMessage msg) = generatePythonMessageCode msg
generatePythonNestedTypeCode (NestedEnum enum) = generatePythonEnumCode enum

-- Python 필드 코드 생성
generatePythonFieldCode :: Field -> String
generatePythonFieldCode field =
    let fieldNameStr = unpack (fieldName field)
        fieldTypeStr = generatePythonFieldTypeCode (fieldType field)
        repeatedTypeStr = if fieldRule field == Repeated
                         then "List[" ++ fieldTypeStr ++ "]"
                         else fieldTypeStr
    in "  " ++ fieldNameStr ++ ": " ++ repeatedTypeStr

-- Python 필드 타입 코드 생성
generatePythonFieldTypeCode :: FieldType -> String
generatePythonFieldTypeCode (ScalarType scalarType) = generatePythonScalarTypeCode scalarType
generatePythonFieldTypeCode (UserDefinedType name) = unpack name
generatePythonFieldTypeCode (MapType keyType valueType) =
    "Dict[" ++ generatePythonScalarTypeCode keyType ++ ", " ++ generatePythonScalarTypeCode valueType ++ "]"

-- Python 스칼라 타입 코드를 Python 타입 문자열로 변환
generatePythonScalarTypeCode :: ProtobufScalarType -> String
generatePythonScalarTypeCode DoubleType = "float"
generatePythonScalarTypeCode FloatType = "float"
generatePythonScalarTypeCode Int32Type = "int"
generatePythonScalarTypeCode Int64Type = "int"
generatePythonScalarTypeCode UInt32Type = "int"
generatePythonScalarTypeCode UInt64Type = "int"
generatePythonScalarTypeCode SInt32Type = "int"
generatePythonScalarTypeCode SInt64Type = "int"
generatePythonScalarTypeCode Fixed32Type = "int"
generatePythonScalarTypeCode Fixed64Type = "int"
generatePythonScalarTypeCode SFixed32Type = "int"
generatePythonScalarTypeCode SFixed64Type = "int"
generatePythonScalarTypeCode BoolType = "bool"
generatePythonScalarTypeCode StringType = "str"
generatePythonScalarTypeCode BytesType = "bytes"

-- Python 열거형 코드 생성
generatePythonEnumCode :: ProtobufEnum -> [String]
generatePythonEnumCode enum =
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = map generatePythonEnumValueCode values
    in [unwords ["class", typeName, "(Enum):"]] ++
        map ("  " ++) valueStrings ++
        [""]

-- Python 열거형 값 코드 생성
generatePythonEnumValueCode :: EnumValue -> String
generatePythonEnumValueCode enumValue =
    let name = unpack (enumValueName enumValue)
        number = enumValueNumber enumValue
    in name ++ " = " ++ show number

-- Python 서비스 코드 생성
generatePythonServiceCode :: Service -> [String]
generatePythonServiceCode service =
    let className = unpack (serviceName service)
        methods = serviceMethods service
        methodStrings = map generatePythonMethodCode methods
    in [unwords ["class", className ++ ":"]] ++
        map ("  " ++) methodStrings ++
        [""]

-- Python 메서드 코드 생성
generatePythonMethodCode :: Method -> String
generatePythonMethodCode method =
    let methodNameStr = unpack (methodName method)
        inputTypeStr = capitalizePythonTypeName (unpack (methodInputType method))
        outputTypeStr = capitalizePythonTypeName (unpack (methodOutputType method))
    in unwords ["def", methodNameStr ++ "(self, input: " ++ inputTypeStr ++ ") -> " ++ outputTypeStr ++ ":"]

-- Python 타입 이름을 대문자로 변환
capitalizePythonTypeName :: String -> String
capitalizePythonTypeName "int32" = "int"
capitalizePythonTypeName "int64" = "int"
capitalizePythonTypeName "uint32" = "int"
capitalizePythonTypeName "uint64" = "int"
capitalizePythonTypeName "string" = "str"
capitalizePythonTypeName "bool" = "bool"
capitalizePythonTypeName "double" = "float"
capitalizePythonTypeName "float" = "float"
capitalizePythonTypeName name = name -- 사용자 정의 타입은 그대로

-- 5. 확장성 지원 함수들

-- 새로운 언어 추가를 위한 헬퍼 함수들
addLanguageGenerator :: String -> CodeGenerator -> [(String, CodeGenerator)] -> [(String, CodeGenerator)]
addLanguageGenerator name generator existing = (name, generator) : existing

-- 지원되는 언어 목록 가져오기
getSupportedLanguages :: [String]
getSupportedLanguages = map fst codeGenerators

-- 언어가 지원되는지 확인
isLanguageSupported :: String -> Bool
isLanguageSupported lang = isJust (lookup lang codeGenerators)

{-
새로운 언어 추가 방법:

1. SimpleMain.hs에서 supportedLanguages에 새 언어 추가:
   ("java", LanguageInfo "java" ".java" "Java")

2. SimpleCodeGen.hs에서 codeGenerators에 새 생성기 추가:
   ("java", generateJavaCode)

3. 새로운 코드 생성 함수 구현:
   generateJavaCode :: ProtobufFile -> String
   generateJavaCode file = ...

4. 필요한 타입 매핑 함수들 구현:
   generateJavaScalarTypeCode :: ProtobufScalarType -> String
   generateJavaFieldCode :: Field -> String
   등등...

예시 - Java 추가:
supportedLanguages = 
    [ ("haskell", LanguageInfo "haskell" ".hs" "Haskell")
    , ("cpp", LanguageInfo "cpp" ".hpp" "C++")
    , ("csharp", LanguageInfo "csharp" ".cs" "C#")
    , ("java", LanguageInfo "java" ".java" "Java")  -- 새로 추가
    ]

codeGenerators =
    [ ("haskell", generateHaskellCode)
    , ("cpp", generateCppCode)
    , ("csharp", generateCSharpCode)
    , ("java", generateJavaCode)  -- 새로 추가
    ]
-}

-- 5. 유틸리티 함수들

-- Data.List.intercalate를 사용하므로 별도 구현 불필요