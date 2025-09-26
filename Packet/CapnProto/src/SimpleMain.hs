{-# LANGUAGE OverloadedStrings #-}

module Main where

import CapnProto.SimpleTypes
import CapnProto.SimpleParser
import CapnProto.SimpleCodeGen
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import qualified Data.Text as T
import Text.Megaparsec (parse, errorBundlePretty)
import Options.Applicative (Parser, ParserInfo, argument, option, str, metavar, help, long, short, optional, switch, info, fullDesc, progDesc, header, helper, execParser, (<**>), auto)
import qualified Options.Applicative as Opt

-- CLI 옵션 정의
data Options = Options
    { inputFile :: String
    , outputLanguage :: String
    , outputFile :: Maybe String
    , verbose :: Bool
    } deriving (Show)

-- CLI 파서
optionsParser :: Opt.Parser Options
optionsParser = Options
    <$> argument str (metavar "INPUT" <> help "Input .capnp file")
    <*> option str (long "language" <> short 'l' <> metavar "LANG" <> help "Output language (haskell, cpp, csharp, python)")
    <*> optional (option str (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file path"))
    <*> switch (long "verbose" <> short 'v' <> help "Verbose output")

-- 메인 함수
main :: IO ()
main = do
    options <- execParser opts
    let inputFilePath = inputFile options
        outputLang = outputLanguage options
        outputFilePath = outputFile options
        verboseFlag = verbose options
    
    if verboseFlag
        then putStrLn $ "Reading input file: " ++ inputFilePath
        else return ()
    
    -- .capnp 파일 읽기
    content <- TIO.readFile inputFilePath
    
    -- 파싱
    case parse parseCapnProtoFile inputFilePath content of
        Left err -> do
            putStrLn "Parse error:"
            putStrLn $ errorBundlePretty err
        Right capnpFile -> do
            if verboseFlag
                then putStrLn "Successfully parsed Cap'n Proto file"
                else return ()
            
            -- 코드 생성
            let generatedCode = generateCode outputLang capnpFile
            
            if verboseFlag
                then do
                    putStrLn "Generated code:"
                    putStrLn generatedCode
                else return ()
            
            -- 출력 파일 결정
            let outputPath = case outputFilePath of
                    Just path -> path
                    Nothing -> "generated/" ++ (takeBaseName inputFilePath) ++ getExtension outputLang
            
            -- 출력 디렉토리 생성
            createDirectoryIfMissing True (takeDirectory outputPath)
            
            -- 파일 쓰기
            writeFile outputPath generatedCode
            putStrLn $ "Code written to: " ++ outputPath
            putStrLn $ "Successfully generated " ++ outputLang ++ " code from Cap'n Proto file"

-- 파일 확장자 결정
getExtension :: String -> String
getExtension "haskell" = ".hs"
getExtension "cpp" = ".hpp"
getExtension "csharp" = ".cs"
getExtension "python" = ".py"
getExtension _ = ".txt"

-- 파일명에서 확장자 제거
takeBaseName :: String -> String
takeBaseName = reverse . dropWhile (/='.') . reverse

-- CLI 옵션 정보
opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
    ( fullDesc
    <> progDesc "Cap'n Proto DSL Packet Generator"
    <> header "capnproto-generator - Generate code from .capnp files"
    )
