module Lib
    ( someFunc
    , greetUser
    , readFileContent
    , writeToFile
    , safeReadFile
    , analyzeFile
    , menu
    , transformFile
    , copyFile
    , analyzeLogFile
    , searchInFile
    ) where

import System.IO
import System.Environment
import Control.Exception
import Data.Char

-- 기본 IO 예제
-- 사용자에게 인사하기
greetUser :: IO ()
greetUser = do
    putStrLn "이름을 입력하세요:"
    name <- getLine
    putStrLn $ "안녕하세요, " ++ name ++ "님!"

-- 파일 읽기/쓰기 예제
-- 파일 내용 읽기
readFileContent :: FilePath -> IO String
readFileContent path = do
    handle <- openFile path ReadMode
    content <- hGetContents handle
    hClose handle
    return content

-- 파일에 내용 쓰기
writeToFile :: FilePath -> String -> IO ()
writeToFile path content = do
    handle <- openFile path WriteMode
    hPutStr handle content
    hClose handle

-- 안전한 파일 처리
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    case result of
        Left _ -> return $ Left $ "파일을 읽을 수 없습니다: " ++ path
        Right content -> return $ Right content

-- 텍스트 파일 통계 분석
analyzeFile :: FilePath -> IO ()
analyzeFile path = do
    content <- readFile path
    let lines = length (lines content)
        words = length (words content)
        chars = length content
    putStrLn $ "파일: " ++ path
    putStrLn $ "줄 수: " ++ show lines
    putStrLn $ "단어 수: " ++ show words
    putStrLn $ "문자 수: " ++ show chars

-- 대화형 메뉴 시스템
menu :: IO ()
menu = do
    putStrLn "\n=== 텍스트 처리 도구 ==="
    putStrLn "1. 파일 분석"
    putStrLn "2. 파일 복사"
    putStrLn "3. 텍스트 변환"
    putStrLn "4. 종료"
    putStr "선택하세요 (1-4): "
    choice <- getLine
    case choice of
        "1" -> do
            putStr "파일명을 입력하세요: "
            filename <- getLine
            analyzeFile filename
            menu
        "2" -> do
            putStr "원본 파일명: "
            source <- getLine
            putStr "대상 파일명: "
            dest <- getLine
            copyFile source dest
            putStrLn "파일이 복사되었습니다."
            menu
        "3" -> do
            putStr "파일명을 입력하세요: "
            filename <- getLine
            transformFile filename
            menu
        "4" -> putStrLn "프로그램을 종료합니다."
        _ -> do
            putStrLn "잘못된 선택입니다."
            menu

-- 파일 변환 함수
transformFile :: FilePath -> IO ()
transformFile path = do
    content <- readFile path
    let transformed = map toUpper content
    writeFile (path ++ ".upper") transformed
    putStrLn "파일이 대문자로 변환되어 저장되었습니다."

-- 파일 복사 함수
copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    content <- readFile source
    writeFile dest content

-- 로그 파일 분석기
analyzeLogFile :: FilePath -> IO ()
analyzeLogFile path = do
    content <- readFile path
    let logLines = lines content
        errorLines = filter (isInfixOf "ERROR") logLines
        warningLines = filter (isInfixOf "WARNING") logLines
        infoLines = filter (isInfixOf "INFO") logLines
    putStrLn $ "로그 파일 분석: " ++ path
    putStrLn $ "전체 로그 수: " ++ show (length logLines)
    putStrLn $ "에러 수: " ++ show (length errorLines)
    putStrLn $ "경고 수: " ++ show (length warningLines)
    putStrLn $ "정보 수: " ++ show (length infoLines)

-- 파일 검색 도구
searchInFile :: FilePath -> String -> IO ()
searchInFile path searchTerm = do
    content <- readFile path
    let lines = lines content
        matchingLines = filter (isInfixOf searchTerm) lines
    putStrLn $ "검색어 '" ++ searchTerm ++ "'를 찾았습니다:"
    mapM_ putStrLn matchingLines

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 입출력과 IO 모나드 예제 ==="
    putStrLn ""
    
    -- 기본 IO 예제
    putStrLn "기본 IO 예제:"
    putStrLn "Hello, Haskell IO!"
    putStrLn $ "현재 시간을 시뮬레이션: " ++ show (42 :: Int)
    putStrLn ""
    
    -- 파일 처리 예제 (시뮬레이션)
    putStrLn "파일 처리 예제 (시뮬레이션):"
    putStrLn "파일 읽기/쓰기 함수들이 정의되었습니다."
    putStrLn "실제 파일을 사용하려면 menu 함수를 호출하세요."
    putStrLn ""
    
    -- 안전한 파일 처리 예제
    putStrLn "안전한 파일 처리:"
    result <- safeReadFile "nonexistent.txt"
    case result of
        Left error -> putStrLn $ "에러: " ++ error
        Right content -> putStrLn $ "내용: " ++ content
    putStrLn ""
    
    -- 메뉴 시스템 시작
    putStrLn "메뉴 시스템을 시작합니다..."
    menu
