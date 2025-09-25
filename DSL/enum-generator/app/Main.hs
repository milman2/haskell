module Main (main) where

import EnumDSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.FilePath
import System.Directory
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> processFile inputFile
        [inputFile, outputDir] -> processFileWithOutput inputFile outputDir
        _ -> do
            putStrLn "Usage: enum-generator <input-file> [output-directory]"
            putStrLn "Example: enum-generator enums.txt output/"
            putStrLn ""
            putStrLn "Input file format:"
            putStrLn "enum Color : uint8 {"
            putStrLn "    RED,"
            putStrLn "    GREEN,"
            putStrLn "    BLUE,"
            putStrLn "}"
            putStrLn ""
            putStrLn "enum WeekOfDay : uint8 {"
            putStrLn "    MONDAY = 0,"
            putStrLn "    TUESDAY,"
            putStrLn "    WEDNESDAY,"
            putStrLn "    THURSDAY,"
            putStrLn "    FRIDAY,"
            putStrLn "    SATURDAY,"
            putStrLn "    SUNDAY,"
            putStrLn "}"

processFile :: FilePath -> IO ()
processFile inputFile = do
    content <- TIO.readFile inputFile
    case parseEnumFile content of
        Left err -> do
            putStrLn $ "Parse error: " ++ show err
        Right enums -> do
            let (cppCode, csharpCode) = generateAll enums
            let baseName = takeBaseName inputFile
            
            -- C++ 파일 생성
            TIO.writeFile (baseName ++ ".hpp") cppCode
            putStrLn $ "Generated C++ header: " ++ baseName ++ ".hpp"
            
            -- C# 파일 생성
            TIO.writeFile (baseName ++ ".cs") csharpCode
            putStrLn $ "Generated C# file: " ++ baseName ++ ".cs"
            
            -- 생성된 코드 미리보기
            putStrLn "\n=== Generated C++ Code ==="
            TIO.putStrLn cppCode
            putStrLn "\n=== Generated C# Code ==="
            TIO.putStrLn csharpCode

processFileWithOutput :: FilePath -> FilePath -> IO ()
processFileWithOutput inputFile outputDir = do
    content <- TIO.readFile inputFile
    case parseEnumFile content of
        Left err -> do
            putStrLn $ "Parse error: " ++ show err
        Right enums -> do
            let (cppCode, csharpCode) = generateAll enums
            let baseName = takeBaseName inputFile
            
            -- 출력 디렉토리 생성
            createDirectoryIfMissing True outputDir
            
            -- C++ 파일 생성
            let cppFile = outputDir </> baseName ++ ".hpp"
            TIO.writeFile cppFile cppCode
            putStrLn $ "Generated C++ header: " ++ cppFile
            
            -- C# 파일 생성
            let csharpFile = outputDir </> baseName ++ ".cs"
            TIO.writeFile csharpFile csharpCode
            putStrLn $ "Generated C# file: " ++ csharpFile
