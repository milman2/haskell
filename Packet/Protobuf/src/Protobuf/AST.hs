{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Protobuf.AST where

import Data.Text (Text)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Protobuf.Types

-- 1. GADT를 사용한 타입 안전한 AST

-- AST 노드 타입을 나타내는 태그
data ASTNodeType
    = FileNode
    | MessageNode
    | FieldNode
    | EnumNode
    | EnumValueNode
    | ServiceNode
    | MethodNode

-- 타입 안전한 AST 노드
data ASTNode (t :: ASTNodeType) where
    -- 파일 노드
    FileAST :: ProtobufFile -> ASTNode FileNode
    
    -- 메시지 노드
    MessageAST :: Message -> ASTNode MessageNode
    
    -- 필드 노드
    FieldAST :: Field -> ASTNode FieldNode
    
    -- 열거형 노드
    EnumAST :: ProtobufEnum -> ASTNode EnumNode
    
    -- 열거형 값 노드
    EnumValueAST :: EnumValue -> ASTNode EnumValueNode
    
    -- 서비스 노드
    ServiceAST :: Service -> ASTNode ServiceNode
    
    -- 메서드 노드
    MethodAST :: Method -> ASTNode MethodNode

-- 2. 타입 지우기 (존재 타입)
data SomeASTNode where
    SomeASTNode :: ASTNode t -> SomeASTNode

-- 3. AST 노드 접근자들

-- 파일 노드 접근
getFile :: ASTNode FileNode -> ProtobufFile
getFile (FileAST file) = file

-- 메시지 노드 접근
getMessage :: ASTNode MessageNode -> Message
getMessage (MessageAST msg) = msg

-- 필드 노드 접근
getField :: ASTNode FieldNode -> Field
getField (FieldAST field) = field

-- 열거형 노드 접근
getEnum :: ASTNode EnumNode -> ProtobufEnum
getEnum (EnumAST enum) = enum

-- 열거형 값 노드 접근
getEnumValue :: ASTNode EnumValueNode -> EnumValue
getEnumValue (EnumValueAST value) = value

-- 서비스 노드 접근
getService :: ASTNode ServiceNode -> Service
getService (ServiceAST service) = service

-- 메서드 노드 접근
getMethod :: ASTNode MethodNode -> Method
getMethod (MethodAST method) = method

-- 4. AST 변환 함수들

-- ProtobufFile을 AST로 변환
fileToAST :: ProtobufFile -> ASTNode FileNode
fileToAST = FileAST

-- Message를 AST로 변환
messageToAST :: Message -> ASTNode MessageNode
messageToAST = MessageAST

-- Field를 AST로 변환
fieldToAST :: Field -> ASTNode FieldNode
fieldToAST = FieldAST

-- ProtobufEnum을 AST로 변환
enumToAST :: ProtobufEnum -> ASTNode EnumNode
enumToAST = EnumAST

-- EnumValue를 AST로 변환
enumValueToAST :: EnumValue -> ASTNode EnumValueNode
enumValueToAST = EnumValueAST

-- Service를 AST로 변환
serviceToAST :: Service -> ASTNode ServiceNode
serviceToAST = ServiceAST

-- Method를 AST로 변환
methodToAST :: Method -> ASTNode MethodNode
methodToAST = MethodAST

-- 5. AST 검증 함수들

-- AST 노드 유효성 검사
validateASTNode :: SomeASTNode -> Either Text SomeASTNode
validateASTNode (SomeASTNode (FileAST file)) = 
    if validateFile file
    then Right (SomeASTNode (FileAST file))
    else Left "Invalid file AST node"
validateASTNode (SomeASTNode (MessageAST msg)) = 
    if validateMessage msg
    then Right (SomeASTNode (MessageAST msg))
    else Left "Invalid message AST node"
validateASTNode (SomeASTNode (FieldAST field)) = 
    if validateField field
    then Right (SomeASTNode (FieldAST field))
    else Left "Invalid field AST node"
validateASTNode (SomeASTNode (EnumAST enum)) = 
    if validateEnum enum
    then Right (SomeASTNode (EnumAST enum))
    else Left "Invalid enum AST node"
validateASTNode (SomeASTNode (EnumValueAST value)) = 
    if validateEnumValue value
    then Right (SomeASTNode (EnumValueAST value))
    else Left "Invalid enum value AST node"
validateASTNode (SomeASTNode (ServiceAST service)) = 
    if validateService service
    then Right (SomeASTNode (ServiceAST service))
    else Left "Invalid service AST node"
validateASTNode (SomeASTNode (MethodAST method)) = 
    if validateMethod method
    then Right (SomeASTNode (MethodAST method))
    else Left "Invalid method AST node"

-- 파일 유효성 검사
validateFile :: ProtobufFile -> Bool
validateFile file = 
    not (null (fileDefinitions file)) &&
    all validateFileDefinition (fileDefinitions file)

-- 파일 정의 유효성 검사
validateFileDefinition :: FileDefinition -> Bool
validateFileDefinition (FileMessage msg) = validateMessage msg
validateFileDefinition (FileEnum enum) = validateEnum enum
validateFileDefinition (FileService service) = validateService service

-- 메시지 유효성 검사
validateMessage :: Message -> Bool
validateMessage msg = 
    isValidMessageName (messageName msg) &&
    all validateField (messageFields msg) &&
    all validateNestedType (messageNestedTypes msg)

-- 중첩된 타입 유효성 검사
validateNestedType :: NestedType -> Bool
validateNestedType (NestedMessage msg) = validateMessage msg
validateNestedType (NestedEnum enum) = validateEnum enum
validateNestedType (NestedService service) = validateService service

-- 필드 유효성 검사
validateField :: Field -> Bool
validateField field = 
    isValidFieldName (fieldName field) &&
    isValidFieldNumber (fieldNumber field) &&
    validateFieldType (fieldType field)

-- 필드 타입 유효성 검사
validateFieldType :: FieldType -> Bool
validateFieldType (ScalarType _) = True
validateFieldType (UserDefinedType name) = not (null name)
validateFieldType (MapType keyType valueType) = 
    validateFieldType (ScalarType StringType) &&  -- Map key는 string이어야 함
    validateFieldType valueType

-- 열거형 유효성 검사
validateEnum :: ProtobufEnum -> Bool
validateEnum enum = 
    isValidEnumName (enumName enum) &&
    not (null (enumValues enum)) &&
    all validateEnumValue (enumValues enum)

-- 열거형 값 유효성 검사
validateEnumValue :: EnumValue -> Bool
validateEnumValue value = 
    not (null (enumValueName value)) &&
    enumValueNumber value >= 0

-- 서비스 유효성 검사
validateService :: Service -> Bool
validateService service = 
    isValidServiceName (serviceName service) &&
    all validateMethod (serviceMethods service)

-- 메서드 유효성 검사
validateMethod :: Method -> Bool
validateMethod method = 
    not (null (methodName method)) &&
    not (null (methodInputType method)) &&
    not (null (methodOutputType method))

-- 6. AST 조작 함수들

-- AST 노드에서 이름 추출
extractName :: SomeASTNode -> Maybe Text
extractName (SomeASTNode (FileAST _)) = Nothing
extractName (SomeASTNode (MessageAST msg)) = Just (messageName msg)
extractName (SomeASTNode (FieldAST field)) = Just (fieldName field)
extractName (SomeASTNode (EnumAST enum)) = Just (enumName enum)
extractName (SomeASTNode (EnumValueAST value)) = Just (enumValueName value)
extractName (SomeASTNode (ServiceAST service)) = Just (serviceName service)
extractName (SomeASTNode (MethodAST method)) = Just (methodName method)

-- AST 노드 타입 확인
isMessageNode :: SomeASTNode -> Bool
isMessageNode (SomeASTNode (MessageAST _)) = True
isMessageNode _ = False

isFieldNode :: SomeASTNode -> Bool
isFieldNode (SomeASTNode (FieldAST _)) = True
isFieldNode _ = False

isEnumNode :: SomeASTNode -> Bool
isEnumNode (SomeASTNode (EnumAST _)) = True
isEnumNode _ = False

isServiceNode :: SomeASTNode -> Bool
isServiceNode (SomeASTNode (ServiceAST _)) = True
isServiceNode _ = False
