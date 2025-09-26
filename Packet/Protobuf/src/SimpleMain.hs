{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Protobuf.SimpleParser as P
import Text.Megaparsec (parse)
import Protobuf.SimpleCodeGen
import Protobuf.SimpleTypes
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.FilePath (takeFileName, takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Options.Applicative (Parser, ParserInfo, argument, option, str, metavar, help, long, short, optional, switch, info, fullDesc, progDesc, header, helper, execParser, (<**>))

-- 1. CLI 옵션 정의

data ProtobufOptions = ProtobufOptions
    { inputFile :: FilePath
    , outputFile :: Maybe FilePath
    , outputDir :: Maybe FilePath
    , verbose :: Bool
    } deriving (Show)

-- 2. CLI 파서

protobufOptions :: Parser ProtobufOptions
protobufOptions = ProtobufOptions
    <$> argument str (metavar "INPUT" <> help "Input .proto file")
    <*> optional (option str (long "output" <> short 'o' <> metavar "FILE" <> help "Output Haskell file"))
    <*> optional (option str (long "output-dir" <> short 'd' <> metavar "DIR" <> help "Output directory (default: generated/)"))
    <*> switch (long "verbose" <> short 'v' <> help "Verbose output")

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
    case parse P.parseProtobufFile "input.proto" inputContent of
        Left parseError -> do
            let errorMsg = "Parse error: " ++ show parseError
            return $ Left errorMsg
        Right protobufFile -> do
            if verbose options
            then putStrLn "Successfully parsed Protocol Buffers file"
            else return ()
            
            -- Haskell 코드 생성
            let haskellCode = generateHaskellCode protobufFile
            
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
                            baseName = takeWhile (/= '.') (takeFileName inputPath)
                            dir = case outputDir options of
                                Just d -> d
                                Nothing -> "generated"
                        in dir ++ "/" ++ baseName ++ ".hs"
            
            -- 출력 디렉토리 생성
            createDirectoryIfMissing True (takeDirectory outputPath)
            
            -- 파일 쓰기
            writeFile outputPath haskellCode
            
            if verbose options
            then putStrLn $ "Code written to: " ++ outputPath
            else return ()
            
            return $ Right ()
