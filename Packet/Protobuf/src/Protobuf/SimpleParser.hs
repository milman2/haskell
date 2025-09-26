{-# LANGUAGE OverloadedStrings #-}

module Protobuf.SimpleParser where

import Protobuf.SimpleTypes
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

-- 파서 타입 정의
type Parser = Parsec Void Text

-- 공백 처리
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- 토큰 파서
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- 심볼 파서
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

-- 키워드 파서
keyword :: Text -> Parser Text
keyword = L.symbol spaceConsumer

-- 식별자 파서
identifier :: Parser Text
identifier = lexeme $ do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    return $ pack (first : rest)

-- 문자열 리터럴 파서
stringLiteral :: Parser Text
stringLiteral = lexeme $ do
    char '"'
    content <- many (noneOf ['"', '\\'] <|> escapedChar)
    char '"'
    return $ pack content
  where
    escapedChar = do
        char '\\'
        c <- anyChar
        return c

-- 숫자 파서
number :: Parser Int
number = lexeme L.decimal

-- 파일 파서
parseFile :: Parser ProtobufFile
parseFile = do
    syntax <- parseSyntax
    package <- optional parsePackage
    definitions <- many parseFileDefinition
    return $ ProtobufFile syntax package definitions

-- syntax 파서
parseSyntax :: Parser Text
parseSyntax = do
    keyword "syntax"
    char '='
    syntax <- stringLiteral
    char ';'
    return syntax

-- package 파서
parsePackage :: Parser Text
parsePackage = do
    keyword "package"
    package <- identifier
    char ';'
    return package

-- 파일 정의 파서
parseFileDefinition :: Parser FileDefinition
parseFileDefinition = 
    FileMessage <$> parseMessage <|>
    FileEnum <$> parseEnum <|>
    FileService <$> parseService

-- 메시지 파서
parseMessage :: Parser Message
parseMessage = do
    keyword "message"
    name <- identifier
    char '{'
    fields <- many parseField
    char '}'
    return $ Message name fields

-- 필드 파서
parseField :: Parser Field
parseField = do
    rule <- parseFieldRule
    fieldType <- parseFieldType
    name <- identifier
    char '='
    number <- number
    char ';'
    return $ Field rule fieldType name number

-- 필드 규칙 파서
parseFieldRule :: Parser FieldRule
parseFieldRule = 
    (keyword "required" >> return Required) <|>
    (keyword "optional" >> return Optional) <|>
    (keyword "repeated" >> return Repeated) <|>
    return Optional

-- 필드 타입 파서
parseFieldType :: Parser FieldType
parseFieldType = 
    ScalarType <$> parseScalarType <|>
    UserDefinedType <$> identifier

-- 스칼라 타입 파서
parseScalarType :: Parser ProtobufScalarType
parseScalarType = 
    (keyword "double" >> return DoubleType) <|>
    (keyword "float" >> return FloatType) <|>
    (keyword "int32" >> return Int32Type) <|>
    (keyword "int64" >> return Int64Type) <|>
    (keyword "uint32" >> return UInt32Type) <|>
    (keyword "uint64" >> return UInt64Type) <|>
    (keyword "sint32" >> return SInt32Type) <|>
    (keyword "sint64" >> return SInt64Type) <|>
    (keyword "fixed32" >> return Fixed32Type) <|>
    (keyword "fixed64" >> return Fixed64Type) <|>
    (keyword "sfixed32" >> return SFixed32Type) <|>
    (keyword "sfixed64" >> return SFixed64Type) <|>
    (keyword "bool" >> return BoolType) <|>
    (keyword "string" >> return StringType) <|>
    (keyword "bytes" >> return BytesType)

-- 열거형 파서
parseEnum :: Parser ProtobufEnum
parseEnum = do
    keyword "enum"
    name <- identifier
    char '{'
    values <- many parseEnumValue
    char '}'
    return $ ProtobufEnum name values

-- 열거형 값 파서
parseEnumValue :: Parser EnumValue
parseEnumValue = do
    name <- identifier
    char '='
    number <- number
    char ';'
    return $ EnumValue name number

-- 서비스 파서
parseService :: Parser Service
parseService = do
    keyword "service"
    name <- identifier
    char '{'
    methods <- many parseMethod
    char '}'
    return $ Service name methods

-- 메서드 파서
parseMethod :: Parser Method
parseMethod = do
    keyword "rpc"
    name <- identifier
    char '('
    inputType <- identifier
    char ')'
    keyword "returns"
    char '('
    outputType <- identifier
    char ')'
    char ';'
    return $ Method name inputType outputType

-- 메인 파서 함수
parseProtobufFile :: Text -> Either (ParseErrorBundle Text Void) ProtobufFile
parseProtobufFile input = parse parseFile "" input