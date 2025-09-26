{-# LANGUAGE OverloadedStrings #-}

module CapnProto.SimpleParser where

import CapnProto.SimpleTypes
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Data.Maybe (isJust)
import Data.Void
import Text.Megaparsec (optional)

type Parser = Parsec Void Text

-- 공백 및 주석 처리
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- 심볼 파싱
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- 키워드 파싱
keyword :: Text -> Parser Text
keyword = L.lexeme spaceConsumer . string

-- 식별자 파싱
identifier :: Parser Text
identifier = L.lexeme spaceConsumer $ do
    first <- letterChar
    rest <- many (alphaNumChar <|> char '_')
    return $ pack (first:rest)

-- 숫자 파싱
number :: Parser Int
number = L.lexeme spaceConsumer L.decimal

-- 문자열 파싱
stringLiteral :: Parser Text
stringLiteral = L.lexeme spaceConsumer $ do
    char '"'
    content <- manyTill L.charLiteral (char '"')
    return $ pack content

-- 전체 파일 파싱
parseCapnProtoFile :: Parser CapnProtoFile
parseCapnProtoFile = spaceConsumer *> do
    imports <- many parseImport
    namespace <- optional parseNamespace
    definitions <- many parseDefinition
    eof
    return $ CapnProtoFile namespace definitions imports

-- Import 파싱
parseImport :: Parser Text
parseImport = do
    keyword "using"
    name <- identifier
    symbol ";"
    return name

-- Namespace 파싱
parseNamespace :: Parser Text
parseNamespace = do
    keyword "namespace"
    name <- identifier
    symbol ";"
    return name

-- 정의 파싱
parseDefinition :: Parser FileDefinition
parseDefinition = choice
    [ FileStruct <$> parseStruct
    , FileInterface <$> parseInterface
    , FileEnum <$> parseEnum
    , FileConst <$> parseConst
    ]

-- Struct 파싱
parseStruct :: Parser Struct
parseStruct = do
    keyword "struct"
    name <- identifier
    symbol "{"
    fields <- many parseField
    groups <- many parseGroup
    unions <- many parseUnion
    symbol "}"
    return $ Struct name fields groups unions

-- Group 파싱
parseGroup :: Parser Group
parseGroup = do
    keyword "group"
    name <- identifier
    symbol "{"
    fields <- many parseField
    symbol "}"
    return $ Group name fields

-- Union 파싱
parseUnion :: Parser Union
parseUnion = do
    keyword "union"
    name <- identifier
    symbol "{"
    types <- many (identifier <* symbol ";")
    symbol "}"
    return $ Union name types

-- Interface 파싱
parseInterface :: Parser Interface
parseInterface = do
    keyword "interface"
    name <- identifier
    symbol "{"
    methods <- many parseMethod
    symbol "}"
    return $ Interface name methods

-- Method 파싱
parseMethod :: Parser Method
parseMethod = do
    name <- identifier
    symbol "("
    params <- many parseMethodParam
    symbol ")"
    symbol "->"
    resultType <- parseFieldType
    symbol ";"
    return $ Method name 0 params (Field resultType "result" 0 Nothing False) -- ID는 나중에 처리

-- Method 파라미터 파싱
parseMethodParam :: Parser Field
parseMethodParam = do
    paramType <- parseFieldType
    paramName <- identifier
    optional (symbol ",")
    return $ Field paramType paramName 0 Nothing False

-- Enum 파싱
parseEnum :: Parser CapnProtoEnum
parseEnum = do
    keyword "enum"
    name <- identifier
    symbol "{"
    values <- many parseEnumValue
    symbol "}"
    return $ CapnProtoEnum name values

-- Enum 값 파싱
parseEnumValue :: Parser EnumValue
parseEnumValue = do
    name <- identifier
    symbol "@"
    enumNumber <- number
    symbol ";"
    return $ EnumValue name (Just enumNumber)

-- Const 파싱
parseConst :: Parser Const
parseConst = do
    keyword "const"
    name <- identifier
    symbol "="
    value <- number
    symbol ";"
    return $ Const name (ScalarType UInt32Type) (pack (show value))

-- 필드 파싱
parseField :: Parser Field
parseField = do
    name <- identifier
    symbol "@"
    fieldId <- number
    symbol ":"
    fieldType <- parseFieldType
    optionalFlag <- optional (keyword "optional")
    defaultVal <- optional (symbol "=" *> stringLiteral)
    symbol ";"
    return $ Field fieldType name fieldId defaultVal (isJust optionalFlag)

-- 필드 타입 파싱
parseFieldType :: Parser FieldType
parseFieldType = choice
    [ ListType <$> (keyword "List" *> symbol "(" *> parseFieldType <* symbol ")")
    , ScalarType <$> parseScalarType
    , UserDefinedType <$> identifier
    , UnionType <$> (keyword "union" *> identifier)
    ]

-- 스칼라 타입 파싱
parseScalarType :: Parser CapnProtoScalarType
parseScalarType = choice
    [ keyword "Void" >> return VoidType
    , keyword "Bool" >> return BoolType
    , keyword "Int8" >> return Int8Type
    , keyword "Int16" >> return Int16Type
    , keyword "Int32" >> return Int32Type
    , keyword "Int64" >> return Int64Type
    , keyword "UInt8" >> return UInt8Type
    , keyword "UInt16" >> return UInt16Type
    , keyword "UInt32" >> return UInt32Type
    , keyword "UInt64" >> return UInt64Type
    , keyword "Float32" >> return Float32Type
    , keyword "Float64" >> return Float64Type
    , keyword "Text" >> return TextType
    , keyword "Data" >> return DataType
    ]
