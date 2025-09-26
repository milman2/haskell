{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified FlatBuffers.SimpleParser as P
import Text.Megaparsec (parse, errorBundlePretty)
import FlatBuffers.SimpleCodeGen
import FlatBuffers.SimpleTypes
import Data.Text (Text, pack, unpack)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intercalate)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.FilePath (takeFileName, takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Options.Applicative (Parser, ParserInfo, argument, option, str, metavar, help, long, short, optional, switch, info, fullDesc, progDesc, header, helper, execParser, (<**>))

-- 1. CLI 옵션 정의

-- 언어 정보를 담는 타입
data LanguageInfo = LanguageInfo
    { langName :: String
    , langExtension :: String
    , langDisplayName :: String
    } deriving (Show, Eq)

-- 지원되는 언어들
supportedLanguages :: [(String, LanguageInfo)]
supportedLanguages =
    [ ("haskell", LanguageInfo "haskell" ".hs" "Haskell")
    , ("cpp", LanguageInfo "cpp" ".hpp" "C++")
    , ("csharp", LanguageInfo "csharp" ".cs" "C#")
    , ("python", LanguageInfo "python" ".py" "Python")
    ]

-- 언어 이름으로 LanguageInfo 찾기
findLanguageInfo :: String -> Maybe LanguageInfo
findLanguageInfo name = lookup name supportedLanguages

-- 지원되는 언어 이름 목록
supportedLanguageNames :: [String]
supportedLanguageNames = map fst supportedLanguages

-- 언어 이름 검증
isValidLanguage :: String -> Bool
isValidLanguage name = isJust (findLanguageInfo name)

data FlatBuffersOptions = FlatBuffersOptions
    { inputFile :: FilePath
    , outputFile :: Maybe FilePath
    , outputDir :: Maybe FilePath
    , language :: String
    , verbose :: Bool
    } deriving (Show)

-- 2. CLI 파서

flatbuffersOptions :: Parser FlatBuffersOptions
flatbuffersOptions = FlatBuffersOptions
    <$> argument str (metavar "INPUT" <> help "Input .fbs file")
    <*> optional (option str (long "output" <> short 'o' <> metavar "FILE" <> help "Output file"))
    <*> optional (option str (long "output-dir" <> short 'd' <> metavar "DIR" <> help "Output directory (default: generated/)"))
    <*> option str (long "language" <> short 'l' <> metavar "LANG" <> help ("Output language: " ++ intercalate ", " supportedLanguageNames ++ " (default: haskell)"))
    <*> switch (long "verbose" <> short 'v' <> help "Verbose output")

cliInfo :: ParserInfo FlatBuffersOptions
cliInfo = info (flatbuffersOptions <**> helper)
    ( fullDesc
    <> progDesc "Generate code from FlatBuffers schema files"
    <> header "flatbuffers-generator - A FlatBuffers DSL packet generator" )

-- 3. 메인 함수

main :: IO ()
main = do
    options <- execParser cliInfo
    result <- processFlatBuffersFile options
    case result of
        Left err -> do
            putStrLn $ "Error: " ++ err
            exitWith (ExitFailure 1)
        Right _ -> do
            putStrLn "Successfully generated code from FlatBuffers schema file"

-- 4. FlatBuffers 파일 처리

processFlatBuffersFile :: FlatBuffersOptions -> IO (Either String ())
processFlatBuffersFile options = do
    -- 입력 파일 읽기
    inputContent <- TIO.readFile (inputFile options)

    if verbose options
    then putStrLn $ "Reading input file: " ++ inputFile options
    else return ()

    -- 파일 파싱
    case parse P.parseFlatBuffersFile "input.fbs" inputContent of
        Left parseError -> do
            let errorMsg = "Parse error: " ++ errorBundlePretty parseError
            return $ Left errorMsg
        Right flatbuffersFile -> do
            if verbose options
            then putStrLn "Successfully parsed FlatBuffers schema file"
            else return ()

            -- 언어 검증
            let langName = map toLower (language options)
            when (not (isValidLanguage langName)) $ do
                putStrLn $ "Error: Unsupported language '" ++ langName ++ "'"
                putStrLn $ "Supported languages: " ++ intercalate ", " supportedLanguageNames
                exitWith (ExitFailure 1)

            -- 코드 생성 (언어별)
            let generatedCode = generateCode langName flatbuffersFile

            if verbose options
            then putStrLn "Generated code:"
            else return ()

            if verbose options
            then putStrLn generatedCode
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
                            extension = case findLanguageInfo langName of
                                Just langInfo -> langExtension langInfo
                                Nothing -> ".hs" -- fallback
                        in dir ++ "/" ++ baseName ++ extension

            -- 출력 디렉토리 생성
            createDirectoryIfMissing True (takeDirectory outputPath)

            -- 파일 쓰기
            TIO.writeFile outputPath (pack generatedCode)

            if verbose options
            then putStrLn $ "Code written to: " ++ outputPath
            else return ()

            return $ Right ()
