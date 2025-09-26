{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module CapnProto.SimpleTypes where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

-- Cap'n Proto 기본 스칼라 타입
data CapnProtoScalarType
    = VoidType | BoolType
    | Int8Type | Int16Type | Int32Type | Int64Type
    | UInt8Type | UInt16Type | UInt32Type | UInt64Type
    | Float32Type | Float64Type
    | TextType | DataType
    deriving (Show, Eq, Generic, Enum, Bounded)

-- 필드 타입 (스칼라, 사용자 정의, 리스트, 유니온)
data FieldType
    = ScalarType CapnProtoScalarType
    | UserDefinedType Text -- Struct, Interface, Enum 이름
    | ListType FieldType -- List<Type> 형태
    | UnionType Text -- Union 이름
    deriving (Show, Eq, Generic)

-- 필드 정의
data Field = Field
    { fieldType :: FieldType
    , fieldName :: Text
    , fieldId :: Int -- @0x123 같은 ID
    , fieldDefault :: Maybe Text -- 기본값
    , fieldOptional :: Bool -- optional 여부
    } deriving (Show, Eq, Generic)

-- Struct 정의
data Struct = Struct
    { structName :: Text
    , structFields :: [Field]
    , structGroups :: [Group] -- 그룹 필드들
    , structUnions :: [Union] -- Union 필드들
    } deriving (Show, Eq, Generic)

-- Group 정의 (Cap'n Proto의 그룹)
data Group = Group
    { groupName :: Text
    , groupFields :: [Field]
    } deriving (Show, Eq, Generic)

-- Union 정의
data Union = Union
    { unionName :: Text
    , unionTypes :: [Text] -- Union에 포함된 타입들
    } deriving (Show, Eq, Generic)

-- Interface 정의 (RPC용)
data Interface = Interface
    { interfaceName :: Text
    , interfaceMethods :: [Method]
    } deriving (Show, Eq, Generic)

-- Method 정의 (RPC 메서드)
data Method = Method
    { methodName :: Text
    , methodId :: Int -- @0x123 같은 ID
    , methodParams :: [Field] -- 파라미터들
    , methodResult :: Field -- 결과 타입
    } deriving (Show, Eq, Generic)

-- Enum 정의
data CapnProtoEnum = CapnProtoEnum
    { enumName :: Text
    , enumValues :: [EnumValue]
    } deriving (Show, Eq, Generic)

-- Enum 값
data EnumValue = EnumValue
    { enumValueName :: Text
    , enumValueNumber :: Maybe Int -- 명시적 값 (없으면 자동 할당)
    } deriving (Show, Eq, Generic)

-- Const 정의
data Const = Const
    { constName :: Text
    , constType :: FieldType
    , constValue :: Text
    } deriving (Show, Eq, Generic)

-- 파일 정의 (Struct, Interface, Enum, Const)
data FileDefinition
    = FileStruct Struct
    | FileInterface Interface
    | FileEnum CapnProtoEnum
    | FileConst Const
    deriving (Show, Eq, Generic)

-- Cap'n Proto 파일
data CapnProtoFile = CapnProtoFile
    { fileNamespace :: Maybe Text
    , fileDefinitions :: [FileDefinition]
    , fileImports :: [Text] -- import 문들
    } deriving (Show, Eq, Generic)
