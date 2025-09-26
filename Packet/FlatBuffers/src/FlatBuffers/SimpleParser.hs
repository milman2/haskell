{-# LANGUAGE OverloadedStrings #-}

module FlatBuffers.SimpleParser where

import FlatBuffers.SimpleTypes
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Control.Monad (void)

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

-- 식별자 파서: 알파벳으로 시작하고 알파벳, 숫자, 밑줄, 점을 포함
identifier :: Parser Text
identifier = lexeme $ do
    h <- letterChar <|> char '_'
    t <- many (alphaNumChar <|> char '_' <|> char '.')
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
    _ <- char '"'
    content <- many (noneOf ['"', '\\'] <|> escapedChar)
    _ <- char '"'
    return $ pack content
  where
    escapedChar = do
        _ <- char '\\'
        c <- oneOf ['\\', '"', 'n', 'r', 't']
        return c

-- 2. FlatBuffers 문법 파서

-- 스칼라 타입 파서
parseFlatBuffersScalarType :: Parser FlatBuffersScalarType
parseFlatBuffersScalarType =
    (keyword "byte" >> return ByteType) <|>
    (keyword "ubyte" >> return UByteType) <|>
    (keyword "bool" >> return BoolType) <|>
    (keyword "short" >> return ShortType) <|>
    (keyword "ushort" >> return UShortType) <|>
    (keyword "int" >> return IntType) <|>
    (keyword "uint" >> return UIntType) <|>
    (keyword "long" >> return LongType) <|>
    (keyword "ulong" >> return ULongType) <|>
    (keyword "float" >> return FloatType) <|>
    (keyword "double" >> return DoubleType) <|>
    (keyword "string" >> return StringType)

-- 필드 타입 파서
parseFieldType :: Parser FieldType
parseFieldType =
    (ScalarType <$> parseFlatBuffersScalarType) <|>
    (UserDefinedType <$> identifier) <|>
    (do
        _ <- symbol "["
        innerType <- parseFieldType
        _ <- symbol "]"
        return $ VectorType innerType
    )

-- 기본값 파서
parseDefaultValue :: Parser Text
parseDefaultValue = do
    _ <- symbol "="
    value <- stringLiteral <|> (pack . show <$> integer) <|> (pack . show <$> float) <|> identifier
    return value

-- deprecated 파서
parseDeprecated :: Parser Bool
parseDeprecated = do
    _ <- symbol "("
    _ <- keyword "deprecated"
    _ <- symbol ")"
    return True

-- 필드 파서
parseField :: Parser Field
parseField = do
    name <- identifier
    _ <- symbol ":"
    typ <- parseFieldType
    defaultVal <- optional parseDefaultValue
    deprecated <- optional parseDeprecated
    _ <- symbol ";"
    return $ Field typ name defaultVal (maybe False id deprecated)

-- Struct 파서
parseStruct :: Parser Struct
parseStruct = do
    _ <- keyword "struct"
    name <- identifier
    _ <- symbol "{"
    fields <- many parseField
    _ <- symbol "}"
    return $ Struct name fields

-- Table 파서
parseTable :: Parser Table
parseTable = do
    _ <- keyword "table"
    name <- identifier
    _ <- symbol "{"
    fields <- many parseField
    _ <- symbol "}"
    return $ Table name fields

-- Enum 파서
parseEnum :: Parser FlatBuffersEnum
parseEnum = do
    _ <- keyword "enum"
    name <- identifier
    _ <- symbol ":"
    baseType <- parseFlatBuffersScalarType
    _ <- symbol "{"
    values <- many parseEnumValue
    _ <- symbol "}"
    return $ FlatBuffersEnum name baseType values

-- Enum 값 파서
parseEnumValue :: Parser EnumValue
parseEnumValue = do
    name <- identifier
    explicitValue <- optional (symbol "=" >> integer)
    _ <- optional (symbol ",")
    return $ EnumValue name explicitValue

-- Union 파서
parseUnion :: Parser Union
parseUnion = do
    _ <- keyword "union"
    name <- identifier
    _ <- symbol "{"
    types <- many (identifier <* optional (symbol ","))
    _ <- symbol "}"
    return $ Union name types

-- Namespace 파서
parseNamespace :: Parser Text
parseNamespace = do
    _ <- keyword "namespace"
    namespace <- identifier
    _ <- symbol ";"
    return namespace

-- Root type 파서
parseRootType :: Parser Text
parseRootType = do
    _ <- keyword "root_type"
    rootType <- identifier
    _ <- symbol ";"
    return rootType

-- 파일 정의 파서
parseFileDefinition :: Parser FileDefinition
parseFileDefinition =
    (FileTable <$> parseTable) <|>
    (FileStruct <$> parseStruct) <|>
    (FileEnum <$> parseEnum) <|>
    (FileUnion <$> parseUnion)

-- FlatBuffers 파일 파서
parseFlatBuffersFile :: Parser FlatBuffersFile
parseFlatBuffersFile = do
    spaceConsumer
    namespace <- optional parseNamespace
    definitions <- many parseFileDefinition
    rootType <- optional parseRootType
    eof
    return $ FlatBuffersFile namespace definitions rootType

-- 최상위 파싱 함수
parseFbs :: Text -> Either (ParseErrorBundle Text Void) FlatBuffersFile
parseFbs = parse parseFlatBuffersFile "input.fbs"
