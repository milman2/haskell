{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Protobuf.Parser where

import Protobuf.Types
import Protobuf.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Void
import Control.Monad (void)
import Data.List (nub)

-- 파서 타입 정의
type Parser = Parsec Void Text

-- 1. 기본 파서 컴비네이터들

-- 공백 처리
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- 토큰 파서 (공백 제거)
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

-- 부동소수점 파서
float :: Parser Double
float = lexeme L.float

-- 2. Protobuf 문법 파서들

-- 파일 파서
parseFile :: Parser ProtobufFile
parseFile = do
    syntax <- parseSyntax
    package <- optional parsePackage
    imports <- many parseImport
    options <- many parseFileOption
    definitions <- many parseFileDefinition
    return $ ProtobufFile syntax package imports options definitions

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

-- import 파서
parseImport :: Parser Text
parseImport = do
    keyword "import"
    importPath <- stringLiteral
    char ';'
    return importPath

-- 파일 옵션 파서
parseFileOption :: Parser FileOption
parseFileOption = do
    keyword "option"
    name <- identifier
    char '='
    value <- parseOptionValue
    char ';'
    return $ FileOption name value

-- 파일 정의 파서
parseFileDefinition :: Parser FileDefinition
parseFileDefinition = 
    FileMessage <$> parseMessage <|>
    FileEnum <$> parseEnum <|>
    FileService <$> parseService

-- 3. 메시지 파서

-- 메시지 파서
parseMessage :: Parser Message
parseMessage = do
    keyword "message"
    name <- identifier
    char '{'
    fields <- many parseField
    nestedTypes <- many parseNestedType
    options <- many parseMessageOption
    char '}'
    return $ Message name fields nestedTypes options

-- 중첩된 타입 파서
parseNestedType :: Parser NestedType
parseNestedType = 
    NestedMessage <$> parseMessage <|>
    NestedEnum <$> parseEnum <|>
    NestedService <$> parseService

-- 메시지 옵션 파서
parseMessageOption :: Parser MessageOption
parseMessageOption = do
    keyword "option"
    name <- identifier
    char '='
    value <- parseOptionValue
    char ';'
    return $ MessageOption name value

-- 4. 필드 파서

-- 필드 파서
parseField :: Parser Field
parseField = do
    rule <- parseFieldRule
    fieldType <- parseFieldType
    name <- identifier
    char '='
    number <- number
    options <- many parseFieldOption
    char ';'
    return $ Field rule fieldType name number options

-- 필드 규칙 파서
parseFieldRule :: Parser FieldRule
parseFieldRule = 
    keyword "required" >> return Required <|>
    keyword "optional" >> return Optional <|>
    keyword "repeated" >> return Repeated <|>
    return Optional  -- proto3에서는 기본값

-- 필드 타입 파서
parseFieldType :: Parser FieldType
parseFieldType = 
    ScalarType <$> parseScalarType <|>
    UserDefinedType <$> identifier <|>
    parseMapType

-- 스칼라 타입 파서
parseScalarType :: Parser ProtobufScalarType
parseScalarType = 
    keyword "double" >> return DoubleType <|>
    keyword "float" >> return FloatType <|>
    keyword "int32" >> return Int32Type <|>
    keyword "int64" >> return Int64Type <|>
    keyword "uint32" >> return UInt32Type <|>
    keyword "uint64" >> return UInt64Type <|>
    keyword "sint32" >> return SInt32Type <|>
    keyword "sint64" >> return SInt64Type <|>
    keyword "fixed32" >> return Fixed32Type <|>
    keyword "fixed64" >> return Fixed64Type <|>
    keyword "sfixed32" >> return SFixed32Type <|>
    keyword "sfixed64" >> return SFixed64Type <|>
    keyword "bool" >> return BoolType <|>
    keyword "string" >> return StringType <|>
    keyword "bytes" >> return BytesType

-- 맵 타입 파서
parseMapType :: Parser FieldType
parseMapType = do
    keyword "map"
    char '<'
    keyType <- parseFieldType
    char ','
    valueType <- parseFieldType
    char '>'
    return $ MapType keyType valueType

-- 필드 옵션 파서
parseFieldOption :: Parser FieldOption
parseFieldOption = do
    char '['
    name <- identifier
    char '='
    value <- parseOptionValue
    char ']'
    return $ FieldOption name value

-- 5. 열거형 파서

-- 열거형 파서
parseEnum :: Parser ProtobufEnum
parseEnum = do
    keyword "enum"
    name <- identifier
    char '{'
    values <- many parseEnumValue
    options <- many parseEnumOption
    char '}'
    return $ ProtobufEnum name values options

-- 열거형 값 파서
parseEnumValue :: Parser EnumValue
parseEnumValue = do
    name <- identifier
    char '='
    number <- number
    options <- many parseEnumValueOption
    char ';'
    return $ EnumValue name number options

-- 열거형 옵션 파서
parseEnumOption :: Parser EnumOption
parseEnumOption = do
    keyword "option"
    name <- identifier
    char '='
    value <- parseOptionValue
    char ';'
    return $ EnumOption name value

-- 열거형 값 옵션 파서
parseEnumValueOption :: Parser EnumValueOption
parseEnumValueOption = do
    char '['
    name <- identifier
    char '='
    value <- parseOptionValue
    char ']'
    return $ EnumValueOption name value

-- 6. 서비스 파서

-- 서비스 파서
parseService :: Parser Service
parseService = do
    keyword "service"
    name <- identifier
    char '{'
    methods <- many parseMethod
    options <- many parseServiceOption
    char '}'
    return $ Service name methods options

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
    options <- many parseMethodOption
    char ';'
    return $ Method name inputType outputType options

-- 서비스 옵션 파서
parseServiceOption :: Parser ServiceOption
parseServiceOption = do
    keyword "option"
    name <- identifier
    char '='
    value <- parseOptionValue
    char ';'
    return $ ServiceOption name value

-- 메서드 옵션 파서
parseMethodOption :: Parser MethodOption
parseMethodOption = do
    char '{'
    name <- identifier
    char '='
    value <- parseOptionValue
    char '}'
    return $ MethodOption name value

-- 7. 옵션 값 파서

-- 옵션 값 파서
parseOptionValue :: Parser OptionValue
parseOptionValue = 
    BoolOption <$> parseBool <|>
    IntOption <$> number <|>
    StringOption <$> stringLiteral <|>
    FloatOption <$> float

-- 불린 파서
parseBool :: Parser Bool
parseBool = 
    keyword "true" >> return True <|>
    keyword "false" >> return False

-- 8. 메인 파서 함수

-- 전체 파일 파싱
parseProtobufFile :: Text -> Either (ParseErrorBundle Text Void) ProtobufFile
parseProtobufFile input = parse parseFile "" input

-- 파싱된 파일을 AST로 변환
parseToAST :: Text -> Either (ParseErrorBundle Text Void) (ASTNode FileNode)
parseToAST input = do
    file <- parseProtobufFile input
    return $ fileToAST file

-- 9. 유틸리티 함수들

-- 파싱 에러를 사용자 친화적인 메시지로 변환
formatParseError :: ParseErrorBundle Text Void -> Text
formatParseError bundle = pack $ errorBundlePretty bundle

-- 파싱 성공 여부 확인
isParseSuccess :: Either (ParseErrorBundle Text Void) a -> Bool
isParseSuccess (Right _) = True
isParseSuccess (Left _) = False
