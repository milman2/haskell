# 5단계: 입출력과 IO 모나드

## 학습 목표
- IO 모나드의 개념과 사용법 이해
- 파일 읽기/쓰기 작업
- 사용자 입력 처리
- 명령줄 인수 처리
- 예외 처리와 에러 핸들링

## 학습 내용

### 1. IO 모나드
- 순수 함수와 부수 효과의 분리
- IO 타입과 do 표기법
- `return`과 `>>=` 연산자

### 2. 기본 IO 함수들
- `putStrLn`, `print`, `getLine`
- `readFile`, `writeFile`
- `getArgs` (명령줄 인수)

### 3. 파일 처리
- 텍스트 파일 읽기/쓰기
- 바이너리 파일 처리
- 디렉토리 조작

### 4. 예외 처리
- `try`, `catch` 함수
- `Control.Exception` 모듈
- 안전한 파일 처리

### 5. 대화형 프로그램
- 사용자와의 상호작용
- 메뉴 시스템
- 입력 검증

## 프로젝트: 텍스트 파일 처리기와 간단한 CLI 도구

### 구현할 기능
1. 텍스트 파일 통계 분석기
2. 간단한 텍스트 에디터
3. 파일 검색 도구
4. 로그 파일 분석기

### 예제 코드
```haskell
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

-- 명령줄 인수 처리
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "사용법: program <파일명>"
        (path:_) -> analyzeFile path

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
```

## 연습 문제
1. 사용자로부터 숫자를 입력받아 팩토리얼을 계산하는 프로그램을 작성하세요
2. 텍스트 파일에서 특정 단어의 출현 횟수를 세는 프로그램을 작성하세요
3. 여러 파일을 하나로 합치는 프로그램을 작성하세요
4. 파일의 각 줄에 줄 번호를 추가하는 프로그램을 작성하세요
5. CSV 파일을 읽어서 통계를 출력하는 프로그램을 작성하세요

## 고급 연습 문제
1. 실시간 로그 모니터링 도구를 작성하세요
2. 파일 백업 도구를 작성하세요
3. 간단한 데이터베이스 쿼리 도구를 작성하세요

## 테스트 방법
```bash
# 컴파일
ghc -o textprocessor Main.hs

# 실행
./textprocessor input.txt
./textprocessor --help
```

## 다음 단계
6단계에서는 모듈과 패키지 관리에 대해 학습합니다.
