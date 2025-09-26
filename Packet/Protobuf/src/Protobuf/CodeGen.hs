{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Protobuf.CodeGen where

import Protobuf.Types
import Protobuf.AST
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.List (intercalate)
import Text.PrettyPrint (Doc, text, (<>), (<+>), vcat, hcat, punctuate, comma, parens, braces)
import qualified Text.PrettyPrint as PP

-- 1. Haskell 타입 생성

-- 메시지를 Haskell 데이터 타입으로 변환
generateMessageType :: Message -> Q [Dec]
generateMessageType msg = do
    let typeName = mkName $ unpack (messageName msg)
    let fields = messageFields msg
    let constructors = generateConstructors fields
    let derivings = generateDerivings
    return [DataD [] typeName [] Nothing constructors derivings]

-- 필드에서 생성자 생성
generateConstructors :: [Field] -> [Con]
generateConstructors fields = 
    [RecC (mkName $ unpack (messageName (Message "" [] [] []))) $ map generateField fields]

-- 필드를 Haskell 레코드 필드로 변환
generateField :: Field -> (Name, Bang, Type)
generateField field = 
    let fieldName = mkName $ unpack (fieldName field)
        fieldType = generateFieldType (fieldType field)
        bang = Bang NoSourceUnpackedness NoSourceStrictness
    in (fieldName, bang, fieldType)

-- 필드 타입을 Haskell 타입으로 변환
generateFieldType :: FieldType -> Type
generateFieldType (ScalarType scalarType) = generateScalarType scalarType
generateFieldType (UserDefinedType name) = ConT $ mkName $ unpack name
generateFieldType (MapType keyType valueType) = 
    AppT (AppT (ConT $ mkName "Map") (generateFieldType keyType)) (generateFieldType valueType)

-- 스칼라 타입을 Haskell 타입으로 변환
generateScalarType :: ProtobufScalarType -> Type
generateScalarType DoubleType = ConT $ mkName "Double"
generateScalarType FloatType = ConT $ mkName "Float"
generateScalarType Int32Type = ConT $ mkName "Int32"
generateScalarType Int64Type = ConT $ mkName "Int64"
generateScalarType UInt32Type = ConT $ mkName "Word32"
generateScalarType UInt64Type = ConT $ mkName "Word64"
generateScalarType SInt32Type = ConT $ mkName "Int32"
generateScalarType SInt64Type = ConT $ mkName "Int64"
generateScalarType Fixed32Type = ConT $ mkName "Word32"
generateScalarType Fixed64Type = ConT $ mkName "Word64"
generateScalarType SFixed32Type = ConT $ mkName "Int32"
generateScalarType SFixed64Type = ConT $ mkName "Int64"
generateScalarType BoolType = ConT $ mkName "Bool"
generateScalarType StringType = ConT $ mkName "Text"
generateScalarType BytesType = ConT $ mkName "ByteString"

-- 파생 인스턴스 생성
generateDerivings :: [DerivClause]
generateDerivings = 
    [DerivClause Nothing [ConT $ mkName "Show"]
    ,DerivClause Nothing [ConT $ mkName "Eq"]
    ,DerivClause Nothing [ConT $ mkName "Generic"]
    ]

-- 2. 열거형 생성

-- 열거형을 Haskell 데이터 타입으로 변환
generateEnumType :: ProtobufEnum -> Q [Dec]
generateEnumType enum = do
    let typeName = mkName $ unpack (enumName enum)
    let constructors = map generateEnumConstructor (enumValues enum)
    let derivings = generateDerivings
    return [DataD [] typeName [] Nothing constructors derivings]

-- 열거형 생성자 생성
generateEnumConstructor :: EnumValue -> Con
generateEnumConstructor value = 
    NormalC (mkName $ unpack (enumValueName value)) []

-- 3. 서비스 생성

-- 서비스를 Haskell 타입 클래스로 변환
generateServiceClass :: Service -> Q [Dec]
generateServiceClass service = do
    let className = mkName $ unpack (serviceName service)
    let methods = serviceMethods service
    let methodSigs = map generateMethodSignature methods
    return [ClassD [] className [] [] methodSigs]

-- 메서드 시그니처 생성
generateMethodSignature :: Method -> Dec
generateMethodSignature method = 
    SigD (mkName $ unpack (methodName method)) 
         (generateMethodType (methodInputType method) (methodOutputType method))

-- 메서드 타입 생성
generateMethodType :: Text -> Text -> Type
generateMethodType inputType outputType = 
    ArrowT `AppT` (ConT $ mkName $ unpack inputType) `AppT` 
    (ConT $ mkName "IO" `AppT` (ConT $ mkName $ unpack outputType))

-- 4. 직렬화/역직렬화 함수 생성

-- 메시지의 직렬화 함수 생성
generateEncodeFunction :: Message -> Q [Dec]
generateEncodeFunction msg = do
    let funcName = mkName $ "encode" ++ unpack (messageName msg)
    let messageType = ConT $ mkName $ unpack (messageName msg)
    let body = generateEncodeBody (messageFields msg)
    return [FunD funcName [Clause [VarP $ mkName "msg"] (NormalB body) []]]

-- 인코딩 바디 생성
generateEncodeBody :: [Field] -> Exp
generateEncodeBody fields = 
    AppE (VarE $ mkName "mconcat") (ListE $ map generateFieldEncode fields)

-- 필드 인코딩 생성
generateFieldEncode :: Field -> Exp
generateFieldEncode field = 
    let fieldName = mkName $ unpack (fieldName field)
        fieldNumber = fieldNumber field
        fieldType = fieldType field
    in generateFieldEncodeExpr fieldName fieldNumber fieldType

-- 필드 인코딩 표현식 생성
generateFieldEncodeExpr :: Name -> Int -> FieldType -> Exp
generateFieldEncodeExpr fieldName fieldNumber (ScalarType scalarType) = 
    AppE (AppE (VarE $ mkName "encodeField") (LitE $ IntegerL $ fromIntegral fieldNumber))
         (AppE (VarE $ generateScalarEncoder scalarType) (VarE fieldName))
generateFieldEncodeExpr fieldName fieldNumber (UserDefinedType typeName) = 
    AppE (AppE (VarE $ mkName "encodeField") (LitE $ IntegerL $ fromIntegral fieldNumber))
         (AppE (VarE $ mkName $ "encode" ++ unpack typeName) (VarE fieldName))
generateFieldEncodeExpr fieldName fieldNumber (MapType keyType valueType) = 
    AppE (AppE (VarE $ mkName "encodeField") (LitE $ IntegerL $ fromIntegral fieldNumber))
         (AppE (VarE $ mkName "encodeMap") (VarE fieldName))

-- 스칼라 인코더 이름 생성
generateScalarEncoder :: ProtobufScalarType -> Name
generateScalarEncoder DoubleType = mkName "encodeDouble"
generateScalarEncoder FloatType = mkName "encodeFloat"
generateScalarEncoder Int32Type = mkName "encodeInt32"
generateScalarEncoder Int64Type = mkName "encodeInt64"
generateScalarEncoder UInt32Type = mkName "encodeUInt32"
generateScalarEncoder UInt64Type = mkName "encodeUInt64"
generateScalarEncoder SInt32Type = mkName "encodeSInt32"
generateScalarEncoder SInt64Type = mkName "encodeSInt64"
generateScalarEncoder Fixed32Type = mkName "encodeFixed32"
generateScalarEncoder Fixed64Type = mkName "encodeFixed64"
generateScalarEncoder SFixed32Type = mkName "encodeSFixed32"
generateScalarEncoder SFixed64Type = mkName "encodeSFixed64"
generateScalarEncoder BoolType = mkName "encodeBool"
generateScalarEncoder StringType = mkName "encodeString"
generateScalarEncoder BytesType = mkName "encodeBytes"

-- 5. 메인 코드 생성 함수

-- 전체 파일에서 Haskell 코드 생성
generateHaskellCode :: ProtobufFile -> Q [Dec]
generateHaskellCode file = do
    let definitions = fileDefinitions file
    let imports = generateImports
    let types = concatMap generateDefinitionTypes definitions
    let functions = concatMap generateDefinitionFunctions definitions
    return $ imports ++ types ++ functions

-- 임포트 생성
generateImports :: [Dec]
generateImports = 
    [ImportD Nothing Nothing (mkName "Data.Text") Nothing False False False
    ,ImportD Nothing Nothing (mkName "Data.ByteString") Nothing False False False
    ,ImportD Nothing Nothing (mkName "Data.Int") Nothing False False False
    ,ImportD Nothing Nothing (mkName "Data.Word") Nothing False False False
    ,ImportD Nothing Nothing (mkName "GHC.Generics") Nothing False False False
    ]

-- 정의에서 타입 생성
generateDefinitionTypes :: FileDefinition -> [Dec]
generateDefinitionTypes (FileMessage msg) = generateMessageType msg
generateDefinitionTypes (FileEnum enum) = generateEnumType enum
generateDefinitionTypes (FileService service) = generateServiceClass service

-- 정의에서 함수 생성
generateDefinitionFunctions :: FileDefinition -> [Dec]
generateDefinitionFunctions (FileMessage msg) = generateEncodeFunction msg
generateDefinitionFunctions (FileEnum _) = []
generateDefinitionFunctions (FileService _) = []

-- 6. Pretty Printing

-- 생성된 코드를 문자열로 출력
generateCodeString :: ProtobufFile -> String
generateCodeString file = 
    let definitions = fileDefinitions file
        imports = generateImportsString
        types = concatMap generateDefinitionTypesString definitions
        functions = concatMap generateDefinitionFunctionsString definitions
    in unlines $ imports ++ types ++ functions

-- 임포트 문자열 생성
generateImportsString :: [String]
generateImportsString = 
    ["import Data.Text (Text)"
    ,"import Data.ByteString (ByteString)"
    ,"import Data.Int (Int32, Int64)"
    ,"import Data.Word (Word32, Word64)"
    ,"import GHC.Generics (Generic)"
    ,""
    ]

-- 정의에서 타입 문자열 생성
generateDefinitionTypesString :: FileDefinition -> [String]
generateDefinitionTypesString (FileMessage msg) = generateMessageTypeString msg
generateDefinitionTypesString (FileEnum enum) = generateEnumTypeString enum
generateDefinitionTypesString (FileService service) = generateServiceClassString service

-- 메시지 타입 문자열 생성
generateMessageTypeString :: Message -> [String]
generateMessageTypeString msg = 
    let typeName = unpack (messageName msg)
        fields = messageFields msg
        fieldStrings = map generateFieldString fields
    in [unwords ["data", typeName, "=", typeName, "{"] ++
        map ("  " ++) fieldStrings ++
        ["} deriving (Show, Eq, Generic)"]
        ]

-- 필드 문자열 생성
generateFieldString :: Field -> String
generateFieldString field = 
    let fieldName = unpack (fieldName field)
        fieldType = generateFieldTypeString (fieldType field)
    in unwords [fieldName, "::", fieldType, ","]

-- 필드 타입 문자열 생성
generateFieldTypeString :: FieldType -> String
generateFieldTypeString (ScalarType scalarType) = generateScalarTypeString scalarType
generateFieldTypeString (UserDefinedType name) = unpack name
generateFieldTypeString (MapType keyType valueType) = 
    "Map " ++ generateFieldTypeString keyType ++ " " ++ generateFieldTypeString valueType

-- 스칼라 타입 문자열 생성
generateScalarTypeString :: ProtobufScalarType -> String
generateScalarTypeString DoubleType = "Double"
generateScalarTypeString FloatType = "Float"
generateScalarTypeString Int32Type = "Int32"
generateScalarTypeString Int64Type = "Int64"
generateScalarTypeString UInt32Type = "Word32"
generateScalarTypeString UInt64Type = "Word64"
generateScalarTypeString SInt32Type = "Int32"
generateScalarTypeString SInt64Type = "Int64"
generateScalarTypeString Fixed32Type = "Word32"
generateScalarTypeString Fixed64Type = "Word64"
generateScalarTypeString SFixed32Type = "Int32"
generateScalarTypeString SFixed64Type = "Int64"
generateScalarTypeString BoolType = "Bool"
generateScalarTypeString StringType = "Text"
generateScalarTypeString BytesType = "ByteString"

-- 열거형 타입 문자열 생성
generateEnumTypeString :: ProtobufEnum -> [String]
generateEnumTypeString enum = 
    let typeName = unpack (enumName enum)
        values = enumValues enum
        valueStrings = map (unpack . enumValueName) values
    in [unwords ["data", typeName, "="] ++
        intercalate " | " valueStrings ++
        ["deriving (Show, Eq, Generic)"]
        ]

-- 서비스 클래스 문자열 생성
generateServiceClassString :: Service -> [String]
generateServiceClassString service = 
    let className = unpack (serviceName service)
        methods = serviceMethods service
        methodStrings = map generateMethodString methods
    in [unwords ["class", className, "m where"] ++
        map ("  " ++) methodStrings
        ]

-- 메서드 문자열 생성
generateMethodString :: Method -> String
generateMethodString method = 
    let methodName = unpack (methodName method)
        inputType = unpack (methodInputType method)
        outputType = unpack (methodOutputType method)
    in unwords [methodName, "::", inputType, "->", "m", outputType]

-- 정의에서 함수 문자열 생성
generateDefinitionFunctionsString :: FileDefinition -> [String]
generateDefinitionFunctionsString (FileMessage msg) = generateEncodeFunctionString msg
generateDefinitionFunctionsString (FileEnum _) = []
generateDefinitionFunctionsString (FileService _) = []

-- 인코드 함수 문자열 생성
generateEncodeFunctionString :: Message -> [String]
generateEncodeFunctionString msg = 
    let funcName = "encode" ++ unpack (messageName msg)
        messageType = unpack (messageName msg)
    in [unwords ["encode" ++ messageType, "::", messageType, "->", "ByteString"] ++
        [unwords ["encode" ++ messageType, "msg", "=", "undefined"]]
        ]
