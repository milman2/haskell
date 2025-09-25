{-# LANGUAGE OverloadedStrings #-}

module EnumDSL
    ( EnumDefinition(..)
    , EnumValue(..)
    , EnumType(..)
    , parseEnumFile
    , generateCpp
    , generateCSharp
    , generateAll
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import Text.Parsec.Text
import Data.List (intercalate)

-- 데이터 타입 정의
data EnumType = UInt8 | UInt16 | UInt32 | UInt64 | Int8 | Int16 | Int32 | Int64
    deriving (Show, Eq)

data EnumValue = EnumValue
    { valueName :: Text
    , valueNumber :: Maybe Int
    } deriving (Show, Eq)

data EnumDefinition = EnumDefinition
    { enumName :: Text
    , enumType :: EnumType
    , enumValues :: [EnumValue]
    } deriving (Show, Eq)

-- 파서 정의
parseEnumFile :: Text -> Either ParseError [EnumDefinition]
parseEnumFile input = parse enumFileParser "" input

enumFileParser :: Parser [EnumDefinition]
enumFileParser = many (spaces >> enumDefinitionParser) <* eof

enumDefinitionParser :: Parser EnumDefinition
enumDefinitionParser = do
    _ <- string "enum"
    _ <- spaces
    name <- identifier
    _ <- spaces
    _ <- char ':'
    _ <- spaces
    typeName <- enumTypeParser
    _ <- spaces
    _ <- char '{'
    _ <- spaces
    values <- enumValuesParser
    _ <- spaces
    _ <- char '}'
    _ <- spaces
    _ <- optional (char ',')
    _ <- spaces
    return $ EnumDefinition name typeName values

enumTypeParser :: Parser EnumType
enumTypeParser = do
    typeStr <- many1 (noneOf " \t\n{")
    case typeStr of
        "uint8" -> return UInt8
        "uint16" -> return UInt16
        "uint32" -> return UInt32
        "uint64" -> return UInt64
        "int8" -> return Int8
        "int16" -> return Int16
        "int32" -> return Int32
        "int64" -> return Int64
        _ -> fail $ "Unknown enum type: " ++ typeStr

enumValuesParser :: Parser [EnumValue]
enumValuesParser = sepEndBy enumValueParser (spaces >> char ',' >> spaces)

enumValueParser :: Parser EnumValue
enumValueParser = do
    name <- identifier
    number <- try (do
        _ <- spaces
        _ <- char '='
        _ <- spaces
        num <- many1 digit
        return (Just (read num :: Int))) <|> return Nothing
    return $ EnumValue name number

identifier :: Parser Text
identifier = do
    first <- letter
    rest <- many (letter <|> digit <|> char '_')
    return $ T.pack (first:rest)

-- C++ 코드 생성기
generateCpp :: [EnumDefinition] -> Text
generateCpp enums = T.unlines $ map generateCppEnum enums

generateCppEnum :: EnumDefinition -> Text
generateCppEnum (EnumDefinition name typeName values) = T.unlines
    [ "enum class " <> name <> " : " <> cppTypeName typeName <> " {"
    , T.intercalate ",\n" $ map (generateCppValue name) values
    , "};"
    , ""
    , "// String conversion functions"
    , "inline const char* toString(" <> name <> " value) {"
    , "    switch (value) {"
    ] <> T.unlines (map (generateCppCase name) values) <> T.unlines
    [ "        default: return \"Unknown\";"
    , "    }"
    , "}"
    , ""
    , "// From string conversion function"
    , "inline " <> name <> " fromString(const std::string& str) {"
    ] <> T.unlines (map (generateCppFromStringCase name) values) <> T.unlines
    [ "    return " <> name <> "::" <> (valueName $ head values) <> "; // default"
    , "}"
    , ""
    ]

cppTypeName :: EnumType -> Text
cppTypeName UInt8 = "std::uint8_t"
cppTypeName UInt16 = "std::uint16_t"
cppTypeName UInt32 = "std::uint32_t"
cppTypeName UInt64 = "std::uint64_t"
cppTypeName Int8 = "std::int8_t"
cppTypeName Int16 = "std::int16_t"
cppTypeName Int32 = "std::int32_t"
cppTypeName Int64 = "std::int64_t"

generateCppValue :: Text -> EnumValue -> Text
generateCppValue enumName (EnumValue name Nothing) = "    " <> name
generateCppValue enumName (EnumValue name (Just num)) = "    " <> name <> " = " <> T.pack (show num)

generateCppCase :: Text -> EnumValue -> Text
generateCppCase enumName (EnumValue name _) = 
    "        case " <> enumName <> "::" <> name <> ": return \"" <> name <> "\";"

generateCppFromStringCase :: Text -> EnumValue -> Text
generateCppFromStringCase enumName (EnumValue name _) = 
    "    if (str == \"" <> name <> "\") return " <> enumName <> "::" <> name <> ";"

-- C# 코드 생성기
generateCSharp :: [EnumDefinition] -> Text
generateCSharp enums = T.unlines $ map generateCSharpEnum enums

generateCSharpEnum :: EnumDefinition -> Text
generateCSharpEnum (EnumDefinition name typeName values) = T.unlines
    [ "public enum " <> name <> " : " <> csharpTypeName typeName
    , "{"
    , T.intercalate ",\n" $ map (generateCSharpValue name) values
    , "}"
    , ""
    , "public static class " <> name <> "Extensions"
    , "{"
    , "    public static string ToString(this " <> name <> " value)"
    , "    {"
    , "        return value switch"
    , "        {"
    ] <> T.unlines (map (generateCSharpCase name) values) <> T.unlines
    [ "            _ => \"Unknown\""
    , "        };"
    , "    }"
    , ""
    , "    public static " <> name <> " FromString(string str)"
    , "    {"
    , "        return str switch"
    , "        {"
    ] <> T.unlines (map (generateCSharpFromStringCase name) values) <> T.unlines
    [ "            _ => " <> name <> "." <> (valueName $ head values) <> " // default"
    , "        };"
    , "    }"
    , "}"
    , ""
    ]

csharpTypeName :: EnumType -> Text
csharpTypeName UInt8 = "byte"
csharpTypeName UInt16 = "ushort"
csharpTypeName UInt32 = "uint"
csharpTypeName UInt64 = "ulong"
csharpTypeName Int8 = "sbyte"
csharpTypeName Int16 = "short"
csharpTypeName Int32 = "int"
csharpTypeName Int64 = "long"

generateCSharpValue :: Text -> EnumValue -> Text
generateCSharpValue enumName (EnumValue name Nothing) = "    " <> name
generateCSharpValue enumName (EnumValue name (Just num)) = "    " <> name <> " = " <> T.pack (show num)

generateCSharpCase :: Text -> EnumValue -> Text
generateCSharpCase enumName (EnumValue name _) = 
    "            " <> name <> " => \"" <> name <> "\","

generateCSharpFromStringCase :: Text -> EnumValue -> Text
generateCSharpFromStringCase enumName (EnumValue name _) = 
    "            \"" <> name <> "\" => " <> name <> ","

-- 전체 코드 생성기
generateAll :: [EnumDefinition] -> (Text, Text)
generateAll enums = (generateCpp enums, generateCSharp enums)
