{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module FlatBuffers.SimpleTypes where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

-- FlatBuffers 기본 스칼라 타입
data FlatBuffersScalarType
    = ByteType | UByteType | BoolType
    | ShortType | UShortType | IntType | UIntType
    | LongType | ULongType | FloatType | DoubleType
    | StringType
    deriving (Show, Eq, Generic, Enum, Bounded)

-- 필드 타입 (스칼라, 사용자 정의, 벡터, 유니온)
data FieldType
    = ScalarType FlatBuffersScalarType
    | UserDefinedType Text -- Table, Struct, Enum 이름
    | VectorType FieldType -- [Type] 형태
    | UnionType Text -- Union 이름
    deriving (Show, Eq, Generic)

-- 필드 정의
data Field = Field
    { fieldType :: FieldType
    , fieldName :: Text
    , fieldDefault :: Maybe Text -- 기본값
    , fieldDeprecated :: Bool -- deprecated 여부
    } deriving (Show, Eq, Generic)

-- Struct 정의 (고정 크기 구조체)
data Struct = Struct
    { structName :: Text
    , structFields :: [Field]
    } deriving (Show, Eq, Generic)

-- Table 정의 (가변 크기 테이블)
data Table = Table
    { tableName :: Text
    , tableFields :: [Field]
    } deriving (Show, Eq, Generic)

-- Enum 정의
data FlatBuffersEnum = FlatBuffersEnum
    { enumName :: Text
    , enumBaseType :: FlatBuffersScalarType -- 기본 타입 (byte, short, int 등)
    , enumValues :: [EnumValue]
    } deriving (Show, Eq, Generic)

-- Enum 값
data EnumValue = EnumValue
    { enumValueName :: Text
    , enumValueNumber :: Maybe Int -- 명시적 값 (없으면 자동 할당)
    } deriving (Show, Eq, Generic)

-- Union 정의
data Union = Union
    { unionName :: Text
    , unionTypes :: [Text] -- Union에 포함된 타입들
    } deriving (Show, Eq, Generic)

-- 파일 정의 (Table, Struct, Enum, Union)
data FileDefinition
    = FileTable Table
    | FileStruct Struct
    | FileEnum FlatBuffersEnum
    | FileUnion Union
    deriving (Show, Eq, Generic)

-- FlatBuffers 파일
data FlatBuffersFile = FlatBuffersFile
    { fileNamespace :: Maybe Text
    , fileDefinitions :: [FileDefinition]
    , fileRootType :: Maybe Text -- root_type 선언
    } deriving (Show, Eq, Generic)
