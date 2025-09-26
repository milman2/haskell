{-# LANGUAGE OverloadedStrings #-}

module CapnProto.SimpleCodeGen where

import CapnProto.SimpleTypes
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.List (intercalate, partition, sortBy)
import Data.Ord (comparing)
import Data.Char (toUpper, toLower)

-- 코드 생성기 타입
type CodeGenerator = String -> CapnProtoFile -> String

-- 언어별 코드 생성기 매핑
codeGenerators :: [(String, CodeGenerator)]
codeGenerators =
    [ ("haskell", generateHaskellCode)
    , ("cpp", generateCppCode)
    , ("csharp", generateCSharpCode)
    , ("python", generatePythonCode)
    ]

-- 메인 코드 생성 함수
generateCode :: String -> CapnProtoFile -> String
generateCode lang capnpFile = 
    case lookup lang codeGenerators of
        Just generator -> generator lang capnpFile
        Nothing -> error $ "Unsupported language: " ++ lang

-- 1. Haskell 코드 생성

-- Haskell 코드 생성
generateHaskellCode :: CodeGenerator
generateHaskellCode _ capnpFile = 
    let imports = generateHaskellImports
        definitions = concatMap generateHaskellDefinitionCode (fileDefinitions capnpFile)
    in unlines (imports ++ definitions)

-- Haskell import 문들
generateHaskellImports :: [String]
generateHaskellImports =
    [ "import Data.Text (Text)"
    , "import Data.ByteString (ByteString)"
    , "import Data.Int (Int8, Int16, Int32, Int64)"
    , "import Data.Word (Word8, Word16, Word32, Word64)"
    , "import GHC.Generics (Generic)"
    , ""
    ]

-- Haskell 정의 코드 생성
generateHaskellDefinitionCode :: FileDefinition -> [String]
generateHaskellDefinitionCode (FileStruct struct) = generateHaskellStructCode struct
generateHaskellDefinitionCode (FileInterface interface) = generateHaskellInterfaceCode interface
generateHaskellDefinitionCode (FileEnum enum) = generateHaskellEnumCode enum
generateHaskellDefinitionCode (FileConst constDef) = generateHaskellConstCode constDef

-- Haskell Struct 코드 생성
generateHaskellStructCode :: Struct -> [String]
generateHaskellStructCode struct =
    let typeName = unpack (structName struct)
        fields = structFields struct
        fieldStrings = addCommasToFields (map generateHaskellFieldCode fields)
    in [unwords ["data", typeName, "=", typeName, "{"]] ++
        map ("  " ++) fieldStrings ++
        ["} deriving (Show, Eq, Generic)", ""]

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
generateHaskellFieldTypeCode (ListType fieldType) = "[" ++ generateHaskellFieldTypeCode fieldType ++ "]"
generateHaskellFieldTypeCode (UnionType name) = unpack name

-- Haskell 스칼라 타입 코드 생성
generateHaskellScalarTypeCode :: CapnProtoScalarType -> String
generateHaskellScalarTypeCode VoidType = "()"
generateHaskellScalarTypeCode BoolType = "Bool"
generateHaskellScalarTypeCode Int8Type = "Int8"
generateHaskellScalarTypeCode Int16Type = "Int16"
generateHaskellScalarTypeCode Int32Type = "Int32"
generateHaskellScalarTypeCode Int64Type = "Int64"
generateHaskellScalarTypeCode UInt8Type = "Word8"
generateHaskellScalarTypeCode UInt16Type = "Word16"
generateHaskellScalarTypeCode UInt32Type = "Word32"
generateHaskellScalarTypeCode UInt64Type = "Word64"
generateHaskellScalarTypeCode Float32Type = "Float"
generateHaskellScalarTypeCode Float64Type = "Double"
generateHaskellScalarTypeCode TextType = "Text"
generateHaskellScalarTypeCode DataType = "ByteString"

-- Haskell Interface 코드 생성
generateHaskellInterfaceCode :: Interface -> [String]
generateHaskellInterfaceCode interface =
    let typeName = unpack (interfaceName interface)
        methods = interfaceMethods interface
        methodStrings = map generateHaskellMethodCode methods
    in [unwords ["class", typeName, "m where"]] ++
        map ("  " ++) methodStrings ++
        [""]

-- Haskell Method 코드 생성
generateHaskellMethodCode :: Method -> String
generateHaskellMethodCode method = 
    let methodNameStr = uncapitalize (unpack (methodName method))
        resultTypeStr = generateHaskellFieldTypeCode (fieldType (methodResult method))
    in unwords [methodNameStr, ":: m", resultTypeStr]

-- Haskell Enum 코드 생성
generateHaskellEnumCode :: CapnProtoEnum -> [String]
generateHaskellEnumCode enum =
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = map generateHaskellEnumValueCode values
    in [unwords ["data", typeName, "=", intercalate " | " valueStrings, "deriving (Show, Eq, Generic)", ""]]

-- Haskell Enum 값 코드 생성
generateHaskellEnumValueCode :: EnumValue -> String
generateHaskellEnumValueCode value = 
    let name = unpack (enumValueName value)
        pascalName = capitalize name
    in pascalName

-- Haskell Const 코드 생성
generateHaskellConstCode :: Const -> [String]
generateHaskellConstCode constDef =
    let name = unpack (constName constDef)
        value = unpack (constValue constDef)
    in [unwords [name, "=", value, "::", generateHaskellFieldTypeCode (constType constDef), ""]]

-- 2. C++ 코드 생성

-- C++ 코드 생성
generateCppCode :: CodeGenerator
generateCppCode _ capnpFile = 
    let includes = generateCppIncludes
        namespace = case fileNamespace capnpFile of
            Just ns -> generateCppNamespace (unpack ns)
            Nothing -> []
        -- 타입 선언 순서: enum 먼저, 그 다음 struct, 마지막에 interface
        -- struct들도 의존성 순서로 정렬
        enums = concatMap generateCppDefinitionCode (filter isEnum (fileDefinitions capnpFile))
        structs = concatMap generateCppDefinitionCode (sortStructsByDependency (filter isStruct (fileDefinitions capnpFile)))
        interfaces = concatMap generateCppDefinitionCode (filter isInterface (fileDefinitions capnpFile))
        definitions = enums ++ structs ++ interfaces
        namespaceEnd = case fileNamespace capnpFile of
            Just _ -> ["", "} // namespace"]
            Nothing -> []
    in unlines (includes ++ namespace ++ definitions ++ namespaceEnd)
  where
    isEnum (FileEnum _) = True
    isEnum _ = False
    isStruct (FileStruct _) = True
    isStruct _ = False
    isInterface (FileInterface _) = True
    isInterface _ = False
    
    -- struct들을 의존성 순서로 정렬
    sortStructsByDependency :: [FileDefinition] -> [FileDefinition]
    sortStructsByDependency structs = 
        let structNames = map getStructName structs
            sortedStructs = sortByDependency structs structNames
        in sortedStructs
    
    getStructName (FileStruct struct) = unpack (structName struct)
    getStructName _ = ""
    
    -- 의존성에 따라 struct 정렬 (간단한 휴리스틱)
    sortByDependency :: [FileDefinition] -> [String] -> [FileDefinition]
    sortByDependency structs allNames = 
        let (noDeps, withDeps) = partition hasNoDependencies structs
            sortedNoDeps = sortBy (comparing getStructName) noDeps
            sortedWithDeps = sortBy (comparing getStructName) withDeps
        in sortedNoDeps ++ sortedWithDeps
    
    hasNoDependencies (FileStruct struct) = 
        let fields = structFields struct
            fieldTypes = map (getFieldTypeName . fieldType) fields
            currentStructName = unpack (structName struct)
        in not (any (`elem` fieldTypes) (filter (/= currentStructName) (map getStructName (filter isStruct (fileDefinitions capnpFile)))))
    hasNoDependencies _ = True
    
    getFieldTypeName (UserDefinedType name) = unpack name
    getFieldTypeName (ListType (UserDefinedType name)) = unpack name
    getFieldTypeName _ = ""

-- C++ 인클루드 생성
generateCppIncludes :: [String]
generateCppIncludes = 
    [ "#pragma once"
    , "#include <string>"
    , "#include <vector>"
    , "#include <cstdint>"
    , ""
    ]

-- C++ 네임스페이스 생성
generateCppNamespace :: String -> [String]
generateCppNamespace ns = ["namespace " ++ ns ++ " {", ""]

-- C++ 정의 코드 생성
generateCppDefinitionCode :: FileDefinition -> [String]
generateCppDefinitionCode (FileStruct struct) = generateCppStructCode struct
generateCppDefinitionCode (FileInterface interface) = generateCppInterfaceCode interface
generateCppDefinitionCode (FileEnum enum) = generateCppEnumCode enum
generateCppDefinitionCode (FileConst constDef) = generateCppConstCode constDef

-- C++ Struct 코드 생성
generateCppStructCode :: Struct -> [String]
generateCppStructCode struct =
    let typeName = unpack (structName struct)
        fields = structFields struct
        fieldStrings = map generateCppFieldCode fields
    in [unwords ["struct", typeName, "{"]] ++
        map ("  " ++) fieldStrings ++
        ["};", ""]

-- C++ 필드 코드 생성
generateCppFieldCode :: Field -> String
generateCppFieldCode field =
    let fieldNameStr = unpack (fieldName field)
        safeFieldName = generateSafeFieldName "cpp" fieldNameStr
        fieldTypeStr = generateCppFieldTypeCode (fieldType field)
    in unwords [fieldTypeStr, safeFieldName, ";"]

-- C++ 필드 타입 코드 생성
generateCppFieldTypeCode :: FieldType -> String
generateCppFieldTypeCode (ScalarType scalarType) = generateCppScalarTypeCode scalarType
generateCppFieldTypeCode (UserDefinedType name) = unpack name
generateCppFieldTypeCode (ListType fieldType) = "std::vector<" ++ generateCppFieldTypeCode fieldType ++ ">"
generateCppFieldTypeCode (UnionType name) = unpack name

-- C++ 스칼라 타입 코드 생성
generateCppScalarTypeCode :: CapnProtoScalarType -> String
generateCppScalarTypeCode VoidType = "void"
generateCppScalarTypeCode BoolType = "bool"
generateCppScalarTypeCode Int8Type = "int8_t"
generateCppScalarTypeCode Int16Type = "int16_t"
generateCppScalarTypeCode Int32Type = "int32_t"
generateCppScalarTypeCode Int64Type = "int64_t"
generateCppScalarTypeCode UInt8Type = "uint8_t"
generateCppScalarTypeCode UInt16Type = "uint16_t"
generateCppScalarTypeCode UInt32Type = "uint32_t"
generateCppScalarTypeCode UInt64Type = "uint64_t"
generateCppScalarTypeCode Float32Type = "float"
generateCppScalarTypeCode Float64Type = "double"
generateCppScalarTypeCode TextType = "std::string"
generateCppScalarTypeCode DataType = "std::string"

-- C++ Interface 코드 생성
generateCppInterfaceCode :: Interface -> [String]
generateCppInterfaceCode interface =
    let typeName = unpack (interfaceName interface)
        methods = interfaceMethods interface
        methodStrings = map generateCppMethodCode methods
    in [unwords ["class", typeName, "{"]] ++
        ["public:"] ++
        map ("  " ++) methodStrings ++
        ["};", ""]

-- C++ Method 코드 생성
generateCppMethodCode :: Method -> String
generateCppMethodCode method =
    let methodNameStr = unpack (methodName method)
        resultTypeStr = generateCppFieldTypeCode (fieldType (methodResult method))
    in unwords ["virtual", resultTypeStr, methodNameStr, "() = 0;"]

-- C++ Enum 코드 생성
generateCppEnumCode :: CapnProtoEnum -> [String]
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

-- C++ Const 코드 생성
generateCppConstCode :: Const -> [String]
generateCppConstCode constDef =
    let name = unpack (constName constDef)
        value = unpack (constValue constDef)
        typeStr = generateCppFieldTypeCode (constType constDef)
    in [unwords ["const", typeStr, name, "=", value, ";"], ""]

-- 3. C# 코드 생성

-- C# 코드 생성
generateCSharpCode :: CodeGenerator
generateCSharpCode _ capnpFile = 
    let imports = generateCSharpImports
        namespace = case fileNamespace capnpFile of
            Just ns -> generateCSharpNamespace (unpack ns)
            Nothing -> []
        definitions = concatMap generateCSharpDefinitionCode (fileDefinitions capnpFile)
        namespaceEnd = case fileNamespace capnpFile of
            Just _ -> ["", "}"]
            Nothing -> []
    in unlines (imports ++ namespace ++ definitions ++ namespaceEnd)

-- C# import 문들
generateCSharpImports :: [String]
generateCSharpImports =
    [ "using System;"
    , "using System.Collections.Generic;"
    , ""
    ]

-- C# 네임스페이스 생성
generateCSharpNamespace :: String -> [String]
generateCSharpNamespace ns = ["namespace " ++ ns, "{"]

-- C# 정의 코드 생성
generateCSharpDefinitionCode :: FileDefinition -> [String]
generateCSharpDefinitionCode (FileStruct struct) = generateCSharpStructCode struct
generateCSharpDefinitionCode (FileInterface interface) = generateCSharpInterfaceCode interface
generateCSharpDefinitionCode (FileEnum enum) = generateCSharpEnumCode enum
generateCSharpDefinitionCode (FileConst constDef) = generateCSharpConstCode constDef

-- C# Struct 코드 생성
generateCSharpStructCode :: Struct -> [String]
generateCSharpStructCode struct =
    let typeName = unpack (structName struct)
        fields = structFields struct
        fieldStrings = map generateCSharpFieldCode fields
    in [unwords ["public class", typeName]] ++
        ["{"] ++
        fieldStrings ++
        ["}", ""]

-- C# 필드 코드 생성
generateCSharpFieldCode :: Field -> String
generateCSharpFieldCode field =
    let fieldNameStr = unpack (fieldName field)
        safeFieldName = generateSafeFieldName "csharp" fieldNameStr
        pascalFieldName = toPascalCase safeFieldName
        fieldTypeStr = generateCSharpFieldTypeCode (fieldType field)
    in unwords ["  public", fieldTypeStr, pascalFieldName, "{ get; set; }"]

-- C# 필드 타입 코드 생성
generateCSharpFieldTypeCode :: FieldType -> String
generateCSharpFieldTypeCode (ScalarType scalarType) = generateCSharpScalarTypeCode scalarType
generateCSharpFieldTypeCode (UserDefinedType name) = unpack name
generateCSharpFieldTypeCode (ListType fieldType) = "List<" ++ generateCSharpFieldTypeCode fieldType ++ ">"
generateCSharpFieldTypeCode (UnionType name) = unpack name

-- C# 스칼라 타입 코드 생성
generateCSharpScalarTypeCode :: CapnProtoScalarType -> String
generateCSharpScalarTypeCode VoidType = "void"
generateCSharpScalarTypeCode BoolType = "bool"
generateCSharpScalarTypeCode Int8Type = "sbyte"
generateCSharpScalarTypeCode Int16Type = "short"
generateCSharpScalarTypeCode Int32Type = "int"
generateCSharpScalarTypeCode Int64Type = "long"
generateCSharpScalarTypeCode UInt8Type = "byte"
generateCSharpScalarTypeCode UInt16Type = "ushort"
generateCSharpScalarTypeCode UInt32Type = "uint"
generateCSharpScalarTypeCode UInt64Type = "ulong"
generateCSharpScalarTypeCode Float32Type = "float"
generateCSharpScalarTypeCode Float64Type = "double"
generateCSharpScalarTypeCode TextType = "string"
generateCSharpScalarTypeCode DataType = "byte[]"

-- C# Interface 코드 생성
generateCSharpInterfaceCode :: Interface -> [String]
generateCSharpInterfaceCode interface =
    let typeName = unpack (interfaceName interface)
        methods = interfaceMethods interface
        methodStrings = map generateCSharpMethodCode methods
    in [unwords ["public interface", typeName]] ++
        ["{"] ++
        map ("  " ++) methodStrings ++
        ["}", ""]

-- C# Method 코드 생성
generateCSharpMethodCode :: Method -> String
generateCSharpMethodCode method =
    let methodNameStr = toPascalCase (unpack (methodName method))
        resultTypeStr = generateCSharpFieldTypeCode (fieldType (methodResult method))
    in unwords [resultTypeStr, methodNameStr, "();"]

-- C# Enum 코드 생성
generateCSharpEnumCode :: CapnProtoEnum -> [String]
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

-- C# Const 코드 생성
generateCSharpConstCode :: Const -> [String]
generateCSharpConstCode constDef =
    let name = unpack (constName constDef)
        value = unpack (constValue constDef)
        typeStr = generateCSharpFieldTypeCode (constType constDef)
    in [unwords ["public const", typeStr, name, "=", value, ";"], ""]

-- 4. Python 코드 생성

-- Python 코드 생성
generatePythonCode :: CodeGenerator
generatePythonCode _ capnpFile = 
    let imports = generatePythonImports
        definitions = concatMap generatePythonDefinitionCode (fileDefinitions capnpFile)
    in unlines (imports ++ definitions)

-- Python import 문들
generatePythonImports :: [String]
generatePythonImports =
    [ "from typing import List, Optional, Union"
    , "from dataclasses import dataclass"
    , ""
    ]

-- Python 정의 코드 생성
generatePythonDefinitionCode :: FileDefinition -> [String]
generatePythonDefinitionCode (FileStruct struct) = generatePythonStructCode struct
generatePythonDefinitionCode (FileInterface interface) = generatePythonInterfaceCode interface
generatePythonDefinitionCode (FileEnum enum) = generatePythonEnumCode enum
generatePythonDefinitionCode (FileConst constDef) = generatePythonConstCode constDef

-- Python Struct 코드 생성
generatePythonStructCode :: Struct -> [String]
generatePythonStructCode struct =
    let typeName = unpack (structName struct)
        fields = structFields struct
        fieldStrings = map generatePythonFieldCode fields
    in [unwords ["@dataclass", "class", typeName, ":"]] ++
        map ("  " ++) fieldStrings ++
        [""]

-- Python 필드 코드 생성
generatePythonFieldCode :: Field -> String
generatePythonFieldCode field =
    let fieldNameStr = unpack (fieldName field)
        safeFieldName = generateSafeFieldName "python" fieldNameStr
        fieldTypeStr = generatePythonFieldTypeCode (fieldType field)
    in unwords [safeFieldName, ":", fieldTypeStr]

-- Python 필드 타입 코드 생성
generatePythonFieldTypeCode :: FieldType -> String
generatePythonFieldTypeCode (ScalarType scalarType) = generatePythonScalarTypeCode scalarType
generatePythonFieldTypeCode (UserDefinedType name) = unpack name
generatePythonFieldTypeCode (ListType fieldType) = "List[" ++ generatePythonFieldTypeCode fieldType ++ "]"
generatePythonFieldTypeCode (UnionType name) = unpack name

-- Python 스칼라 타입 코드 생성
generatePythonScalarTypeCode :: CapnProtoScalarType -> String
generatePythonScalarTypeCode VoidType = "None"
generatePythonScalarTypeCode BoolType = "bool"
generatePythonScalarTypeCode Int8Type = "int"
generatePythonScalarTypeCode Int16Type = "int"
generatePythonScalarTypeCode Int32Type = "int"
generatePythonScalarTypeCode Int64Type = "int"
generatePythonScalarTypeCode UInt8Type = "int"
generatePythonScalarTypeCode UInt16Type = "int"
generatePythonScalarTypeCode UInt32Type = "int"
generatePythonScalarTypeCode UInt64Type = "int"
generatePythonScalarTypeCode Float32Type = "float"
generatePythonScalarTypeCode Float64Type = "float"
generatePythonScalarTypeCode TextType = "str"
generatePythonScalarTypeCode DataType = "bytes"

-- Python Interface 코드 생성
generatePythonInterfaceCode :: Interface -> [String]
generatePythonInterfaceCode interface =
    let typeName = unpack (interfaceName interface)
        methods = interfaceMethods interface
        methodStrings = map generatePythonMethodCode methods
    in [unwords ["class", typeName, ":"]] ++
        map ("  " ++) methodStrings ++
        [""]

-- Python Method 코드 생성
generatePythonMethodCode :: Method -> String
generatePythonMethodCode method =
    let methodNameStr = unpack (methodName method)
        resultTypeStr = generatePythonFieldTypeCode (fieldType (methodResult method))
    in unwords ["def", methodNameStr, "(self) ->", resultTypeStr, ":"]

-- Python Enum 코드 생성
generatePythonEnumCode :: CapnProtoEnum -> [String]
generatePythonEnumCode enum =
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = map generatePythonEnumValueCode values
    in [unwords ["class", typeName, ":"]] ++
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

-- Python Const 코드 생성
generatePythonConstCode :: Const -> [String]
generatePythonConstCode constDef =
    let name = unpack (constName constDef)
        value = unpack (constValue constDef)
    in [unwords [name, "=", value, ""]]

-- 유틸리티 함수들

-- 예약어 검사 및 안전한 필드명 생성
haskellReservedWords :: [String]
haskellReservedWords = ["type", "class", "data", "where", "if", "then", "else", "case", "of", "let", "in", "do", "import", "module", "instance", "deriving", "newtype", "typeclass", "forall", "exists"]

cppReservedWords :: [String]
cppReservedWords = ["class", "struct", "enum", "union", "namespace", "public", "private", "protected", "virtual", "static", "const", "volatile", "mutable", "explicit", "inline", "friend", "operator", "template", "typename", "auto", "decltype", "nullptr", "if", "else", "for", "while", "do", "switch", "case", "default", "break", "continue", "return", "goto", "try", "catch", "throw", "new", "delete", "this", "sizeof", "typedef", "extern", "register", "signed", "unsigned", "short", "long", "int", "char", "float", "double", "void", "bool", "true", "false"]

csharpReservedWords :: [String]
csharpReservedWords = ["class", "struct", "enum", "interface", "namespace", "public", "private", "protected", "internal", "virtual", "static", "const", "readonly", "volatile", "mutable", "explicit", "implicit", "inline", "sealed", "abstract", "override", "new", "virtual", "operator", "event", "delegate", "using", "if", "else", "for", "while", "do", "switch", "case", "default", "break", "continue", "return", "goto", "try", "catch", "throw", "finally", "lock", "checked", "unchecked", "unsafe", "fixed", "stackalloc", "sizeof", "typeof", "is", "as", "this", "base", "null", "true", "false", "void", "bool", "byte", "sbyte", "char", "decimal", "double", "float", "int", "uint", "long", "ulong", "object", "short", "ushort", "string", "var", "dynamic", "ref", "out", "params", "in", "where", "select", "from", "group", "orderby", "join", "let", "into", "on", "equals", "by", "ascending", "descending"]

pythonReservedWords :: [String]
pythonReservedWords = ["and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else", "except", "exec", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "not", "or", "pass", "print", "raise", "return", "try", "while", "with", "yield", "True", "False", "None", "type"]

isReservedWord :: String -> String -> Bool
isReservedWord lang word = 
    case lang of
        "haskell" -> word `elem` haskellReservedWords
        "cpp" -> word `elem` cppReservedWords
        "csharp" -> word `elem` csharpReservedWords
        "python" -> word `elem` pythonReservedWords
        _ -> False

generateSafeFieldName :: String -> String -> String
generateSafeFieldName lang fieldName = 
    if isReservedWord lang fieldName
        then fieldName ++ "Field"
        else fieldName

-- 필드들에 콤마 추가 (마지막 필드 제외)
addCommasToFields :: [String] -> [String]
addCommasToFields [] = []
addCommasToFields [x] = [x]
addCommasToFields (x:xs) = (x ++ ",") : addCommasToFields xs

-- Enum 값들에 콤마 추가 (마지막 값 제외)
addCommasToEnumValues :: [String] -> [String]
addCommasToEnumValues [] = []
addCommasToEnumValues [x] = [x]
addCommasToEnumValues (x:xs) = (x ++ ",") : addCommasToEnumValues xs

-- 첫 글자를 소문자로 변환
uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (x:xs) = toLowerChar x : xs
  where toLowerChar c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- 첫 글자를 대문자로 변환
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- snake_case를 PascalCase로 변환
toPascalCase :: String -> String
toPascalCase = capitalize . concat . map capitalize . words . map (\c -> if c == '_' then ' ' else c)
