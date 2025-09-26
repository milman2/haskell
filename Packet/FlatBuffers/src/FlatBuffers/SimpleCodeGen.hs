{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.SimpleCodeGen where

import FlatBuffers.SimpleTypes
import Data.Text (unpack)
import Data.List (intercalate)

-- 코드 생성기 타입
type CodeGenerator = String -> FlatBuffersFile -> String

-- 언어별 코드 생성기 목록
codeGenerators :: [(String, CodeGenerator)]
codeGenerators =
    [ ("haskell", generateHaskellCode)
    , ("cpp", generateCppCode)
    , ("csharp", generateCSharpCode)
    , ("python", generatePythonCode)
    ]

-- 메인 코드 생성 함수
generateCode :: String -> FlatBuffersFile -> String
generateCode lang fbsFile = 
    case lookup lang codeGenerators of
        Just generator -> generator lang fbsFile
        Nothing -> error $ "Unsupported language: " ++ lang

-- 1. Haskell 코드 생성

-- Haskell 코드 생성
generateHaskellCode :: CodeGenerator
generateHaskellCode _ fbsFile = 
    let imports = generateHaskellImports
        definitions = concatMap generateHaskellDefinitionCode (fileDefinitions fbsFile)
    in unlines (imports ++ definitions)

-- Haskell 임포트 생성
generateHaskellImports :: [String]
generateHaskellImports = 
    [ "import Data.Text (Text)"
    , "import Data.ByteString (ByteString)"
    , "import Data.Int (Int8, Int16, Int32, Int64)"
    , "import Data.Word (Word8, Word16, Word32, Word64)"
    , "import Data.Vector (Vector)"
    , "import GHC.Generics (Generic)"
    , ""
    ]

-- Haskell 정의 코드 생성
generateHaskellDefinitionCode :: FileDefinition -> [String]
generateHaskellDefinitionCode (FileTable table) = generateHaskellTableCode table
generateHaskellDefinitionCode (FileStruct struct) = generateHaskellStructCode struct
generateHaskellDefinitionCode (FileEnum enum) = generateHaskellEnumCode enum
generateHaskellDefinitionCode (FileUnion union) = generateHaskellUnionCode union

-- Haskell Table 코드 생성
generateHaskellTableCode :: Table -> [String]
generateHaskellTableCode table =
    let typeName = unpack (tableName table)
        fields = tableFields table
        fieldStrings = map generateHaskellFieldCode fields
        indentedFields = addCommasToFields fieldStrings
    in [unwords ["data", typeName, "=", typeName, "{"]] ++
        indentedFields ++
        ["} deriving (Show, Eq, Generic)", ""]

-- Haskell Struct 코드 생성
generateHaskellStructCode :: Struct -> [String]
generateHaskellStructCode struct =
    let typeName = unpack (structName struct)
        fields = structFields struct
        fieldStrings = map generateHaskellFieldCode fields
        indentedFields = addCommasToFields fieldStrings
    in [unwords ["data", typeName, "=", typeName, "{"]] ++
        indentedFields ++
        ["} deriving (Show, Eq, Generic)", ""]

-- Haskell Enum 코드 생성
generateHaskellEnumCode :: FlatBuffersEnum -> [String]
generateHaskellEnumCode enum =
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = map (capitalize . unpack . enumValueName) values
    in [unwords ["data", typeName, "=", intercalate " | " valueStrings, "deriving (Show, Eq, Generic, Enum, Bounded)", ""]]

-- Haskell Union 코드 생성
generateHaskellUnionCode :: Union -> [String]
generateHaskellUnionCode union =
    let typeName = unpack (unionName union)
        types = unionTypes union
        typeStrings = map unpack types
    in [unwords ["data", typeName, "=", intercalate " | " typeStrings, "deriving (Show, Eq, Generic)", ""]]

-- Haskell 필드 코드 생성
generateHaskellFieldCode :: Field -> String
generateHaskellFieldCode field = 
    let fieldNameStr = unpack (fieldName field)
        safeFieldName = generateSafeFieldName "haskell" fieldNameStr
        fieldTypeStr = generateHaskellFieldTypeCode (fieldType field)
    in unwords [safeFieldName, "::", fieldTypeStr]

-- Haskell 필드 타입 코드 생성
generateHaskellFieldTypeCode :: FieldType -> String
generateHaskellFieldTypeCode (ScalarType scalarType) = generateHaskellScalarTypeCode scalarType
generateHaskellFieldTypeCode (UserDefinedType name) = unpack name
generateHaskellFieldTypeCode (VectorType innerType) = "Vector " ++ generateHaskellFieldTypeCode innerType
generateHaskellFieldTypeCode (UnionType name) = unpack name

-- Haskell 스칼라 타입 코드 생성
generateHaskellScalarTypeCode :: FlatBuffersScalarType -> String
generateHaskellScalarTypeCode ByteType = "Int8"
generateHaskellScalarTypeCode UByteType = "Word8"
generateHaskellScalarTypeCode BoolType = "Bool"
generateHaskellScalarTypeCode ShortType = "Int16"
generateHaskellScalarTypeCode UShortType = "Word16"
generateHaskellScalarTypeCode IntType = "Int32"
generateHaskellScalarTypeCode UIntType = "Word32"
generateHaskellScalarTypeCode LongType = "Int64"
generateHaskellScalarTypeCode ULongType = "Word64"
generateHaskellScalarTypeCode FloatType = "Float"
generateHaskellScalarTypeCode DoubleType = "Double"
generateHaskellScalarTypeCode StringType = "Text"

-- 2. C++ 코드 생성

-- C++ 코드 생성
generateCppCode :: CodeGenerator
generateCppCode _ fbsFile = 
    let includes = generateCppIncludes
        namespace = case fileNamespace fbsFile of
            Just ns -> generateCppNamespace (unpack ns)
            Nothing -> []
        -- 타입 선언 순서: enum 먼저, 그 다음 struct/table, 마지막에 union
        enums = concatMap generateCppDefinitionCode (filter isEnum (fileDefinitions fbsFile))
        structs = concatMap generateCppDefinitionCode (filter isStructOrTable (fileDefinitions fbsFile))
        unions = concatMap generateCppDefinitionCode (filter isUnion (fileDefinitions fbsFile))
        definitions = enums ++ structs ++ unions
        namespaceEnd = case fileNamespace fbsFile of
            Just _ -> ["", "} // namespace"]
            Nothing -> []
    in unlines (includes ++ namespace ++ definitions ++ namespaceEnd)
  where
    isEnum (FileEnum _) = True
    isEnum _ = False
    isStructOrTable (FileStruct _) = True
    isStructOrTable (FileTable _) = True
    isStructOrTable _ = False
    isUnion (FileUnion _) = True
    isUnion _ = False

-- C++ 인클루드 생성
generateCppIncludes :: [String]
generateCppIncludes = 
    [ "#pragma once"
    , "#include <string>"
    , "#include <vector>"
    , "#include <variant>"
    , "#include <cstdint>"
    , ""
    ]

-- C++ 네임스페이스 생성
generateCppNamespace :: String -> [String]
generateCppNamespace ns = 
    [ "namespace " ++ ns ++ " {"
    , ""
    ]

-- C++ 정의 코드 생성
generateCppDefinitionCode :: FileDefinition -> [String]
generateCppDefinitionCode (FileTable table) = generateCppTableCode table
generateCppDefinitionCode (FileStruct struct) = generateCppStructCode struct
generateCppDefinitionCode (FileEnum enum) = generateCppEnumCode enum
generateCppDefinitionCode (FileUnion union) = generateCppUnionCode union

-- C++ Table 코드 생성
generateCppTableCode :: Table -> [String]
generateCppTableCode table =
    let typeName = unpack (tableName table)
        fields = tableFields table
        fieldStrings = map generateCppFieldCode fields
        getterFunctions = generateCppGetterFunctions typeName fields
        setterFunctions = generateCppSetterFunctions typeName fields
    in [unwords ["struct", typeName, "{"]] ++
        ["public:"] ++
        getterFunctions ++
        [""] ++  -- getter와 setter 사이에 빈 줄 하나
        setterFunctions ++
        ["private:"] ++
        fieldStrings ++
        ["};", ""]

-- C++ Struct 코드 생성
generateCppStructCode :: Struct -> [String]
generateCppStructCode struct =
    let typeName = unpack (structName struct)
        fields = structFields struct
        fieldStrings = map generateCppFieldCode fields
    in [unwords ["struct", typeName, "{"]] ++
        fieldStrings ++
        ["};", ""]

-- C++ Enum 코드 생성
generateCppEnumCode :: FlatBuffersEnum -> [String]
generateCppEnumCode enum =
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = addCommasToEnumValues (map generateCppEnumValueCode values)
    in [unwords ["enum class", typeName, "{"]] ++
        map ("  " ++) valueStrings ++
        ["};", ""]

-- C++ Enum 값 코드 생성
generateCppEnumValueCode :: EnumValue -> String
generateCppEnumValueCode value =
    let name = unpack (enumValueName value)
        number = case enumValueNumber value of
            Just n -> " = " ++ show n
            Nothing -> ""
    in name ++ number

-- C++ Union 코드 생성 (std::variant 사용)
generateCppUnionCode :: Union -> [String]
generateCppUnionCode union =
    let typeName = unpack (unionName union)
        types = unionTypes union
        typeStrings = map unpack types
        variantTypes = intercalate ", " typeStrings
    in [unwords ["using", typeName, "= std::variant<" ++ variantTypes ++ ">;", ""]]

-- C++ Getter 함수들 생성
generateCppGetterFunctions :: String -> [Field] -> [String]
generateCppGetterFunctions typeName fields = 
    let getterFuncs = map (generateCppGetterFunction typeName) fields
    in concat getterFuncs

-- C++ 개별 Getter 함수 생성
generateCppGetterFunction :: String -> Field -> [String]
generateCppGetterFunction _ field = 
    let fieldNameStr = unpack (fieldName field)
        safeFieldName = generateSafeFieldName "cpp" fieldNameStr
        fieldTypeStr = generateCppFieldTypeCode (fieldType field)
        getterName = "get" ++ capitalize safeFieldName
    in [unwords ["  " ++ fieldTypeStr, "&", getterName ++ "() { return " ++ safeFieldName ++ "; }"]
       ,unwords ["  const " ++ fieldTypeStr, "&", getterName ++ "() const { return " ++ safeFieldName ++ "; }"]
       ]

-- C++ Setter 함수들 생성
generateCppSetterFunctions :: String -> [Field] -> [String]
generateCppSetterFunctions typeName fields = 
    let setterFuncs = map (generateCppSetterFunction typeName) fields
    in concat setterFuncs

-- C++ 개별 Setter 함수 생성
generateCppSetterFunction :: String -> Field -> [String]
generateCppSetterFunction _ field = 
    let fieldNameStr = unpack (fieldName field)
        safeFieldName = generateSafeFieldName "cpp" fieldNameStr
        fieldTypeStr = generateCppFieldTypeCode (fieldType field)
        setterName = "set" ++ capitalize safeFieldName
        paramName = safeFieldName ++ "Value"
    in [unwords ["  void", setterName ++ "(const " ++ fieldTypeStr ++ "& " ++ paramName ++ ") { " ++ safeFieldName ++ " = " ++ paramName ++ "; }"]
       ]

-- C++ 필드 코드 생성
generateCppFieldCode :: Field -> String
generateCppFieldCode field =
    let fieldNameStr = unpack (fieldName field)
        safeFieldName = generateSafeFieldName "cpp" fieldNameStr
        fieldTypeStr = generateCppFieldTypeCode (fieldType field)
    in "  " ++ fieldTypeStr ++ " " ++ safeFieldName ++ ";"

-- C++ 필드 타입 코드 생성
generateCppFieldTypeCode :: FieldType -> String
generateCppFieldTypeCode (ScalarType scalarType) = generateCppScalarTypeCode scalarType
generateCppFieldTypeCode (UserDefinedType name) = unpack name
generateCppFieldTypeCode (VectorType innerType) = "std::vector<" ++ generateCppFieldTypeCode innerType ++ ">"
generateCppFieldTypeCode (UnionType name) = unpack name

-- C++ 스칼라 타입 코드 생성
generateCppScalarTypeCode :: FlatBuffersScalarType -> String
generateCppScalarTypeCode ByteType = "int8_t"
generateCppScalarTypeCode UByteType = "uint8_t"
generateCppScalarTypeCode BoolType = "bool"
generateCppScalarTypeCode ShortType = "int16_t"
generateCppScalarTypeCode UShortType = "uint16_t"
generateCppScalarTypeCode IntType = "int32_t"
generateCppScalarTypeCode UIntType = "uint32_t"
generateCppScalarTypeCode LongType = "int64_t"
generateCppScalarTypeCode ULongType = "uint64_t"
generateCppScalarTypeCode FloatType = "float"
generateCppScalarTypeCode DoubleType = "double"
generateCppScalarTypeCode StringType = "std::string"

-- 3. C# 코드 생성

-- C# 코드 생성
generateCSharpCode :: CodeGenerator
generateCSharpCode _ fbsFile = 
    let imports = generateCSharpImports
        namespace = case fileNamespace fbsFile of
            Just ns -> generateCSharpNamespace (unpack ns)
            Nothing -> []
        definitions = concatMap generateCSharpDefinitionCode (fileDefinitions fbsFile)
        namespaceEnd = case fileNamespace fbsFile of
            Just _ -> ["", "}"]
            Nothing -> []
    in unlines (imports ++ namespace ++ definitions ++ namespaceEnd)

-- C# 임포트 생성
generateCSharpImports :: [String]
generateCSharpImports = 
    [ "using System;"
    , "using System.Collections.Generic;"
    , ""
    ]

-- C# 네임스페이스 생성
generateCSharpNamespace :: String -> [String]
generateCSharpNamespace ns = 
    [ "namespace " ++ ns
    , "{"
    , ""
    ]

-- C# 정의 코드 생성
generateCSharpDefinitionCode :: FileDefinition -> [String]
generateCSharpDefinitionCode (FileTable table) = generateCSharpTableCode table
generateCSharpDefinitionCode (FileStruct struct) = generateCSharpStructCode struct
generateCSharpDefinitionCode (FileEnum enum) = generateCSharpEnumCode enum
generateCSharpDefinitionCode (FileUnion union) = generateCSharpUnionCode union

-- C# Table 코드 생성
generateCSharpTableCode :: Table -> [String]
generateCSharpTableCode table =
    let typeName = unpack (tableName table)
        fields = tableFields table
        fieldStrings = map generateCSharpFieldCode fields
    in [unwords ["public class", typeName]] ++
        ["{"] ++
        fieldStrings ++
        ["}", ""]

-- C# Struct 코드 생성
generateCSharpStructCode :: Struct -> [String]
generateCSharpStructCode struct =
    let typeName = unpack (structName struct)
        fields = structFields struct
        fieldStrings = map generateCSharpFieldCode fields
    in [unwords ["public struct", typeName]] ++
        ["{"] ++
        fieldStrings ++
        ["}", ""]

-- C# Enum 코드 생성
generateCSharpEnumCode :: FlatBuffersEnum -> [String]
generateCSharpEnumCode enum =
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = addCommasToEnumValues (map generateCSharpEnumValueCode values)
    in [unwords ["public enum", typeName]] ++
        ["{"] ++
        map ("  " ++) valueStrings ++
        ["}", ""]

-- C# Enum 값 코드 생성
generateCSharpEnumValueCode :: EnumValue -> String
generateCSharpEnumValueCode value =
    let name = unpack (enumValueName value)
        number = case enumValueNumber value of
            Just n -> " = " ++ show n
            Nothing -> ""
    in name ++ number

-- C# Enum 값들에 콤마 추가 (마지막 값 제외)
addCommasToEnumValues :: [String] -> [String]
addCommasToEnumValues [] = []
addCommasToEnumValues [x] = [x]
addCommasToEnumValues (x:xs) = (x ++ ",") : addCommasToEnumValues xs

-- C# Union 코드 생성 (abstract class with derived classes)
generateCSharpUnionCode :: Union -> [String]
generateCSharpUnionCode union =
    let typeName = unpack (unionName union)
        types = unionTypes union
        typeStrings = map unpack types
        baseClass = [unwords ["public abstract class", typeName], "{"]
        derivedClasses = concatMap (\t -> [unwords ["  public class", t, ":", typeName], "  {", "  }"]) typeStrings
    in baseClass ++ derivedClasses ++ ["}", ""]

-- C# 필드 코드 생성
generateCSharpFieldCode :: Field -> String
generateCSharpFieldCode field =
    let fieldNameStr = unpack (fieldName field)
        pascalCaseFieldName = toPascalCase fieldNameStr
        safeFieldName = generateSafeFieldName "csharp" pascalCaseFieldName
        fieldTypeStr = generateCSharpFieldTypeCode (fieldType field)
    in "  public " ++ fieldTypeStr ++ " " ++ safeFieldName ++ " { get; set; }"

-- C# 필드 타입 코드 생성
generateCSharpFieldTypeCode :: FieldType -> String
generateCSharpFieldTypeCode (ScalarType scalarType) = generateCSharpScalarTypeCode scalarType
generateCSharpFieldTypeCode (UserDefinedType name) = unpack name
generateCSharpFieldTypeCode (VectorType innerType) = "List<" ++ generateCSharpFieldTypeCode innerType ++ ">"
generateCSharpFieldTypeCode (UnionType name) = unpack name

-- C# 스칼라 타입 코드 생성
generateCSharpScalarTypeCode :: FlatBuffersScalarType -> String
generateCSharpScalarTypeCode ByteType = "sbyte"
generateCSharpScalarTypeCode UByteType = "byte"
generateCSharpScalarTypeCode BoolType = "bool"
generateCSharpScalarTypeCode ShortType = "short"
generateCSharpScalarTypeCode UShortType = "ushort"
generateCSharpScalarTypeCode IntType = "int"
generateCSharpScalarTypeCode UIntType = "uint"
generateCSharpScalarTypeCode LongType = "long"
generateCSharpScalarTypeCode ULongType = "ulong"
generateCSharpScalarTypeCode FloatType = "float"
generateCSharpScalarTypeCode DoubleType = "double"
generateCSharpScalarTypeCode StringType = "string"

-- 4. Python 코드 생성

-- Python 코드 생성
generatePythonCode :: CodeGenerator
generatePythonCode _ fbsFile = 
    let imports = generatePythonImports
        definitions = concatMap generatePythonDefinitionCode (fileDefinitions fbsFile)
    in unlines (imports ++ definitions)

-- Python 임포트 생성
generatePythonImports :: [String]
generatePythonImports = 
    [ "from dataclasses import dataclass"
    , "from typing import List, Optional, Union"
    , "from enum import Enum"
    , ""
    ]

-- Python 정의 코드 생성
generatePythonDefinitionCode :: FileDefinition -> [String]
generatePythonDefinitionCode (FileTable table) = generatePythonTableCode table
generatePythonDefinitionCode (FileStruct struct) = generatePythonStructCode struct
generatePythonDefinitionCode (FileEnum enum) = generatePythonEnumCode enum
generatePythonDefinitionCode (FileUnion union) = generatePythonUnionCode union

-- Python Table 코드 생성
generatePythonTableCode :: Table -> [String]
generatePythonTableCode table =
    let typeName = unpack (tableName table)
        fields = tableFields table
        fieldStrings = map generatePythonFieldCode fields
    in [unwords ["@dataclass", "class", typeName ++ ":"]] ++
        fieldStrings ++
        [""]

-- Python Struct 코드 생성
generatePythonStructCode :: Struct -> [String]
generatePythonStructCode struct =
    let typeName = unpack (structName struct)
        fields = structFields struct
        fieldStrings = map generatePythonFieldCode fields
    in [unwords ["@dataclass", "class", typeName ++ ":"]] ++
        fieldStrings ++
        [""]

-- Python Enum 코드 생성
generatePythonEnumCode :: FlatBuffersEnum -> [String]
generatePythonEnumCode enum =
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = map generatePythonEnumValueCode values
    in [unwords ["class", typeName ++ "(Enum):"]] ++
        map ("  " ++) valueStrings ++
        [""]

-- Python Enum 값 코드 생성
generatePythonEnumValueCode :: EnumValue -> String
generatePythonEnumValueCode value =
    let name = unpack (enumValueName value)
        number = case enumValueNumber value of
            Just n -> " = " ++ show n
            Nothing -> ""
    in name ++ number

-- Python Union 코드 생성 (Union type hint)
generatePythonUnionCode :: Union -> [String]
generatePythonUnionCode union =
    let typeName = unpack (unionName union)
        types = unionTypes union
        typeStrings = map unpack types
        unionTypesStr = intercalate " | " typeStrings
    in [unwords [typeName, "=", unionTypesStr], ""]

-- Python 필드 코드 생성
generatePythonFieldCode :: Field -> String
generatePythonFieldCode field =
    let fieldNameStr = unpack (fieldName field)
        safeFieldName = generateSafeFieldName "python" fieldNameStr
        fieldTypeStr = generatePythonFieldTypeCode (fieldType field)
    in "  " ++ safeFieldName ++ ": " ++ fieldTypeStr

-- Python 필드 타입 코드 생성
generatePythonFieldTypeCode :: FieldType -> String
generatePythonFieldTypeCode (ScalarType scalarType) = generatePythonScalarTypeCode scalarType
generatePythonFieldTypeCode (UserDefinedType name) = unpack name
generatePythonFieldTypeCode (VectorType innerType) = "List[" ++ generatePythonFieldTypeCode innerType ++ "]"
generatePythonFieldTypeCode (UnionType name) = unpack name

-- Python 스칼라 타입 코드 생성
generatePythonScalarTypeCode :: FlatBuffersScalarType -> String
generatePythonScalarTypeCode ByteType = "int"
generatePythonScalarTypeCode UByteType = "int"
generatePythonScalarTypeCode BoolType = "bool"
generatePythonScalarTypeCode ShortType = "int"
generatePythonScalarTypeCode UShortType = "int"
generatePythonScalarTypeCode IntType = "int"
generatePythonScalarTypeCode UIntType = "int"
generatePythonScalarTypeCode LongType = "int"
generatePythonScalarTypeCode ULongType = "int"
generatePythonScalarTypeCode FloatType = "float"
generatePythonScalarTypeCode DoubleType = "float"
generatePythonScalarTypeCode StringType = "str"

-- 5. 공통 유틸리티 함수

-- 필드에 쉼표 추가 (마지막 필드 제외)
addCommasToFields :: [String] -> [String]
addCommasToFields [] = []
addCommasToFields [lastField] = ["  " ++ lastField]
addCommasToFields (field:rest) = ("  " ++ field ++ " ,") : addCommasToFields rest

-- 첫 글자 대문자로 변환
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpperChar x : xs
  where toUpperChar c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- snake_case를 PascalCase로 변환
toPascalCase :: String -> String
toPascalCase [] = []
toPascalCase (x:xs) = toUpperChar x : processRest xs
  where
    toUpperChar c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
    processRest [] = []
    processRest ('_':y:ys) = toUpperChar y : processRest ys
    processRest (y:ys) = y : processRest ys

-- Haskell 예약어 목록
haskellReservedWords :: [String]
haskellReservedWords = 
    ["case", "class", "data", "default", "deriving", "do", "else", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "then", "type", "where", "_", "as", "qualified", "hiding", "foreign", "export", "safe", "unsafe", "ccall", "stdcall", "cplusplus", "jvm", "dotnet", "primitive"]

-- C++ 예약어 목록
cppReservedWords :: [String]
cppReservedWords = 
    ["alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor", "bool", "break", "case", "catch", "char", "char16_t", "char32_t", "class", "compl", "const", "constexpr", "const_cast", "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit", "export", "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private", "protected", "public", "register", "reinterpret_cast", "return", "short", "signed", "sizeof", "static", "static_assert", "static_cast", "struct", "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"]

-- C# 예약어 목록
csharpReservedWords :: [String]
csharpReservedWords = 
    ["abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", "null", "object", "operator", "out", "override", "params", "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using", "virtual", "void", "volatile", "while", "Type", "Class", "Namespace", "Public", "Private", "Protected", "If", "For", "While", "Return"]

-- Python 예약어 목록
pythonReservedWords :: [String]
pythonReservedWords = 
    ["False", "None", "True", "and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else", "except", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise", "return", "try", "while", "with", "yield"]

-- 예약어 검사 함수
isReservedWord :: String -> String -> Bool
isReservedWord "haskell" word = word `elem` haskellReservedWords
isReservedWord "cpp" word = word `elem` cppReservedWords
isReservedWord "csharp" word = word `elem` csharpReservedWords
isReservedWord "python" word = word `elem` pythonReservedWords
isReservedWord _ _ = False

-- 예약어 충돌 시 대체 이름 생성
generateSafeFieldName :: String -> String -> String
generateSafeFieldName lang fieldName
    | isReservedWord lang fieldName = fieldName ++ "Field"
    | otherwise = fieldName
