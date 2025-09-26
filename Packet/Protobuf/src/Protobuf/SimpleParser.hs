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

-- 1. 렉서(Lexer) 정의
-- 주석 및 공백 처리
spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1                          -- 공백
    (L.skipLineComment "//")        -- 한 줄 주석
    (L.skipBlockComment "/*" "*/")  -- 블록 주석

-- 렉심(lexeme) 파서: 파싱 후 공백을 소비
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- 심볼 파서: 특정 문자열을 파싱하고 공백을 소비
symbol :: Text -> Parser Text
symbol = lexeme . string

-- 키워드 파서: 특정 키워드를 파싱하고 식별자와 겹치지 않도록 처리
keyword :: Text -> Parser Text
keyword kw = lexeme (string kw <* notFollowedBy alphaNumChar)

-- 식별자 파서: 알파벳으로 시작하고 알파벳, 숫자, 밑줄을 포함
identifier :: Parser Text
identifier = lexeme $ do
    h <- letterChar <|> char '_'
    t <- many (alphaNumChar <|> char '_')
    return (pack (h:t))

-- 정수 파서
integer :: Parser Int
integer = lexeme L.decimal

-- 실수 파서
float :: Parser Double
float = lexeme L.float

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
        c <- oneOf ['\\', '"', 'n', 'r', 't']
        return c

-- 2. Protobuf 문법 파서

-- 스칼라 타입 파서
parseProtobufScalarType :: Parser ProtobufScalarType
parseProtobufScalarType =
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

-- 필드 규칙 파서
parseFieldRule :: Parser FieldRule
parseFieldRule =
    (keyword "required" >> return Required) <|>
    (keyword "optional" >> return Optional) <|>
    (keyword "repeated" >> return Repeated)

-- 필드 타입 파서
parseFieldType :: Parser FieldType
parseFieldType =
    (ScalarType <$> parseProtobufScalarType) <|>
    (UserDefinedType <$> identifier)

-- 필드 파서
parseField :: Parser Field
parseField = do
    rule <- optional parseFieldRule
    typ <- parseFieldType
    name <- identifier
    symbol "="
    num <- integer
    symbol ";"
    return $ Field (maybe Optional id rule) typ name num

-- 메시지 파서
parseMessage :: Parser Message
parseMessage = do
    keyword "message"
    name <- identifier
    symbol "{"
    fields <- many parseField
    symbol "}"
    return $ Message name fields

-- 열거형 파서
parseEnum :: Parser ProtobufEnum
parseEnum = do
    keyword "enum"
    name <- identifier
    symbol "{"
    values <- many parseEnumValue
    symbol "}"
    return $ ProtobufEnum name values

-- 열거형 값 파서
parseEnumValue :: Parser EnumValue
parseEnumValue = do
    name <- identifier
    symbol "="
    num <- integer
    symbol ";"
    return $ EnumValue name num

-- 서비스 파서
parseService :: Parser Service
parseService = do
    keyword "service"
    name <- identifier
    symbol "{"
    methods <- many parseMethod
    symbol "}"
    return $ Service name methods

-- 서비스 메서드 파서
parseMethod :: Parser Method
parseMethod = do
    keyword "rpc"
    name <- identifier
    symbol "("
    inputType <- identifier
    symbol ")"
    keyword "returns"
    symbol "("
    outputType <- identifier
    symbol ")"
    symbol ";"
    return $ Method name inputType outputType

-- 파일 정의 파서
parseFileDefinition :: Parser FileDefinition
parseFileDefinition =
    (FileMessage <$> parseMessage) <|>
    (FileEnum <$> parseEnum) <|>
    (FileService <$> parseService)

-- syntax 파서
parseSyntax :: Parser Text
parseSyntax = do
    keyword "syntax"
    symbol "="
    syntax <- stringLiteral
    symbol ";"
    return syntax

-- Protobuf 파일 파서
parseProtobufFile :: Parser ProtobufFile
parseProtobufFile = do
    spaceConsumer
    syntax <- optional parseSyntax
    definitions <- many parseFileDefinition
    eof
    return $ ProtobufFile (maybe "proto2" id syntax) Nothing definitions

-- 최상위 파싱 함수
parseProto :: Text -> Either (ParseErrorBundle Text Void) ProtobufFile
parseProto = parse parseProtobufFile "input.proto"