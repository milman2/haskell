{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protobuf.Parser
import Protobuf.AST
import Protobuf.CodeGen
import Protobuf.Serialize
import Protobuf.Types
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Options.Applicative
import Data.Semigroup ((<>))

-- 1. CLI 옵션 정의

data ProtobufOptions = ProtobufOptions
    { inputFile :: FilePath
    , outputFile :: Maybe FilePath
    , outputDir :: Maybe FilePath
    , verbose :: Bool
    , validate :: Bool
    } deriving (Show)

-- 2. CLI 파서

protobufOptions :: Parser ProtobufOptions
protobufOptions = ProtobufOptions
    <$> argument str (metavar "INPUT" <> help "Input .proto file")
    <*> optional (option str (long "output" <> short 'o' <> metavar "FILE" <> help "Output Haskell file"))
    <*> optional (option str (long "output-dir" <> short 'd' <> metavar "DIR" <> help "Output directory"))
    <*> switch (long "verbose" <> short 'v' <> help "Verbose output")
    <*> switch (long "validate" <> help "Validate input file")

-- CLI 정보
cliInfo :: ParserInfo ProtobufOptions
cliInfo = info (protobufOptions <**> helper)
    (fullDesc
     <> progDesc "Generate Haskell code from Protocol Buffers .proto files"
     <> header "protobuf-generator - A Haskell DSL for Protocol Buffers")

-- 3. 메인 함수

main :: IO ()
main = do
    options <- execParser cliInfo
    result <- processProtobufFile options
    case result of
        Left err -> do
            putStrLn $ "Error: " ++ err
            exitWith (ExitFailure 1)
        Right _ -> do
            putStrLn "Successfully generated Haskell code from Protocol Buffers file"

-- 4. Protobuf 파일 처리

processProtobufFile :: ProtobufOptions -> IO (Either String ())
processProtobufFile options = do
    -- 입력 파일 읽기
    inputContent <- TIO.readFile (inputFile options)
    
    if verbose options
    then putStrLn $ "Reading input file: " ++ inputFile options
    else return ()
    
    -- 파일 파싱
    case parseProtobufFile inputContent of
        Left parseError -> do
            let errorMsg = "Parse error: " ++ T.unpack (formatParseError parseError)
            return $ Left errorMsg
        Right protobufFile -> do
            if verbose options
            then putStrLn "Successfully parsed Protocol Buffers file"
            else return ()
            
            -- 검증 (옵션)
            if validate options
            then do
                case validateProtobufFile protobufFile of
                    Left validationError -> return $ Left validationError
                    Right _ -> do
                        if verbose options
                        then putStrLn "File validation passed"
                        else return ()
                        generateHaskellCode protobufFile options
            else generateHaskellCode protobufFile options

-- 5. Protobuf 파일 검증

validateProtobufFile :: ProtobufFile -> Either String ()
validateProtobufFile file = do
    -- 파일 레벨 검증
    validateFileSyntax file
    validateFilePackage file
    validateFileImports file
    
    -- 정의 검증
    mapM_ validateFileDefinition (fileDefinitions file)
    
    return ()

-- 파일 문법 검증
validateFileSyntax :: ProtobufFile -> Either String ()
validateFileSyntax file = 
    let syntax = fileSyntax file
    in if syntax `elem` ["proto2", "proto3"]
       then Right ()
       else Left $ "Invalid syntax: " ++ T.unpack syntax ++ ". Must be 'proto2' or 'proto3'"

-- 파일 패키지 검증
validateFilePackage :: ProtobufFile -> Either String ()
validateFilePackage file = 
    case filePackage file of
        Nothing -> Right ()  -- 패키지는 선택사항
        Just package -> 
            if T.length package > 0
            then Right ()
            else Left "Package name cannot be empty"

-- 파일 임포트 검증
validateFileImports :: ProtobufFile -> Either String ()
validateFileImports file = 
    let imports = fileImports file
    in if all (T.length > 0) imports
       then Right ()
       else Left "Import paths cannot be empty"

-- 파일 정의 검증
validateFileDefinition :: FileDefinition -> Either String ()
validateFileDefinition (FileMessage msg) = validateMessage msg
validateFileDefinition (FileEnum enum) = validateEnum enum
validateFileDefinition (FileService service) = validateService service

-- 메시지 검증
validateMessage :: Message -> Either String ()
validateMessage msg = do
    -- 메시지 이름 검증
    if T.length (messageName msg) > 0
    then Right ()
    else Left "Message name cannot be empty"
    
    -- 필드 검증
    mapM_ validateField (messageFields msg)
    
    -- 중첩된 타입 검증
    mapM_ validateNestedType (messageNestedTypes msg)

-- 중첩된 타입 검증
validateNestedType :: NestedType -> Either String ()
validateNestedType (NestedMessage msg) = validateMessage msg
validateNestedType (NestedEnum enum) = validateEnum enum
validateNestedType (NestedService service) = validateService service

-- 필드 검증
validateField :: Field -> Either String ()
validateField field = do
    -- 필드 이름 검증
    if T.length (fieldName field) > 0
    then Right ()
    else Left "Field name cannot be empty"
    
    -- 필드 번호 검증
    if fieldNumber field > 0 && fieldNumber field <= 536870911
    then Right ()
    else Left $ "Invalid field number: " ++ show (fieldNumber field) ++ ". Must be between 1 and 536870911"

-- 열거형 검증
validateEnum :: Enum -> Either String ()
validateEnum enum = do
    -- 열거형 이름 검증
    if T.length (enumName enum) > 0
    then Right ()
    else Left "Enum name cannot be empty"
    
    -- 열거형 값 검증
    if not (null (enumValues enum))
    then Right ()
    else Left "Enum must have at least one value"
    
    mapM_ validateEnumValue (enumValues enum)

-- 열거형 값 검증
validateEnumValue :: EnumValue -> Either String ()
validateEnumValue value = do
    -- 열거형 값 이름 검증
    if T.length (enumValueName value) > 0
    then Right ()
    else Left "Enum value name cannot be empty"
    
    -- 열거형 값 번호 검증
    if enumValueNumber value >= 0
    then Right ()
    else Left $ "Invalid enum value number: " ++ show (enumValueNumber value) ++ ". Must be non-negative"

-- 서비스 검증
validateService :: Service -> Either String ()
validateService service = do
    -- 서비스 이름 검증
    if T.length (serviceName service) > 0
    then Right ()
    else Left "Service name cannot be empty"
    
    -- 메서드 검증
    mapM_ validateMethod (serviceMethods service)

-- 메서드 검증
validateMethod :: Method -> Either String ()
validateMethod method = do
    -- 메서드 이름 검증
    if T.length (methodName method) > 0
    then Right ()
    else Left "Method name cannot be empty"
    
    -- 입력 타입 검증
    if T.length (methodInputType method) > 0
    then Right ()
    else Left "Method input type cannot be empty"
    
    -- 출력 타입 검증
    if T.length (methodOutputType method) > 0
    then Right ()
    else Left "Method output type cannot be empty"

-- 6. Haskell 코드 생성

generateHaskellCode :: ProtobufFile -> ProtobufOptions -> IO (Either String ())
generateHaskellCode file options = do
    -- 코드 생성
    let haskellCode = generateCodeString file
    
    if verbose options
    then putStrLn "Generated Haskell code:"
    else return ()
    
    if verbose options
    then putStrLn haskellCode
    else return ()
    
    -- 출력 파일 결정
    let outputPath = case outputFile options of
            Just path -> path
            Nothing -> 
                let inputPath = inputFile options
                    baseName = takeWhile (/= '.') inputPath
                in baseName ++ ".hs"
    
    -- 파일 쓰기
    writeFile outputPath haskellCode
    
    if verbose options
    then putStrLn $ "Code written to: " ++ outputPath
    else return ()
    
    return $ Right ()

-- 7. 유틸리티 함수들

-- 에러 메시지 포맷팅
formatError :: String -> String
formatError msg = "Error: " ++ msg

-- 성공 메시지 포맷팅
formatSuccess :: String -> String
formatSuccess msg = "Success: " ++ msg
