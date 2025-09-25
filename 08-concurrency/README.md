# 8단계: 동시성과 병렬성

## 학습 목표
- STM (Software Transactional Memory) 이해
- Async와 Concurrent 프로그래밍
- 병렬 처리와 성능 최적화
- 동시성 제어와 동기화
- 실시간 시스템 프로그래밍

## 학습 내용

### 1. STM (Software Transactional Memory)
- 원자적 트랜잭션
- `TVar`와 `STM` 모나드
- 트랜잭션 충돌 해결

### 2. Async 프로그래밍
- `Async` 타입과 `async` 함수
- 비동기 작업 관리
- 예외 처리와 취소

### 3. 병렬 처리
- `par`와 `pseq`를 사용한 병렬 평가
- `Control.Parallel.Strategies`
- 데이터 병렬성

### 4. 동시성 제어
- `MVar`와 `Chan`
- 세마포어와 뮤텍스
- 데드락 방지

### 5. 실시간 시스템
- 타이머와 스케줄링
- 이벤트 기반 프로그래밍
- 웹 서버 구현

## 프로젝트: 멀티스레드 웹 서버와 병렬 계산

### 구현할 기능
1. STM을 사용한 은행 계좌 시스템
2. 병렬 정렬 알고리즘
3. 멀티스레드 웹 서버
4. 실시간 채팅 서버

### 예제 코드

#### BankAccount.hs (STM을 사용한 은행 계좌)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module BankAccount where

import Control.Concurrent.STM
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T

-- 계좌 타입
data Account = Account
    { accountId :: Text
    , balance :: TVar Int
    , transactions :: TVar [Transaction]
    }

data Transaction = Transaction
    { transactionId :: Text
    , fromAccount :: Text
    , toAccount :: Text
    , amount :: Int
    , timestamp :: Int
    } deriving (Show)

-- 계좌 생성
createAccount :: Text -> Int -> STM Account
createAccount accountId initialBalance = do
    balanceVar <- newTVar initialBalance
    transactionsVar <- newTVar []
    return $ Account accountId balanceVar transactionsVar

-- 잔액 조회
getBalance :: Account -> STM Int
getBalance account = readTVar (balance account)

-- 입금
deposit :: Account -> Int -> STM ()
deposit account amount = do
    currentBalance <- readTVar (balance account)
    writeTVar (balance account) (currentBalance + amount)

-- 출금
withdraw :: Account -> Int -> STM Bool
withdraw account amount = do
    currentBalance <- readTVar (balance account)
    if currentBalance >= amount
        then do
            writeTVar (balance account) (currentBalance - amount)
            return True
        else return False

-- 계좌 간 이체
transfer :: Account -> Account -> Int -> STM Bool
transfer fromAccount toAccount amount = do
    success <- withdraw fromAccount amount
    if success
        then do
            deposit toAccount amount
            return True
        else return False

-- 트랜잭션 기록
recordTransaction :: Account -> Transaction -> STM ()
recordTransaction account transaction = do
    transactions <- readTVar (transactions account)
    writeTVar (transactions account) (transaction : transactions)

-- 원자적 이체 (트랜잭션 포함)
atomicTransfer :: Account -> Account -> Int -> Text -> STM Bool
atomicTransfer fromAccount toAccount amount transactionId = do
    success <- transfer fromAccount toAccount amount
    when success $ do
        let transaction = Transaction transactionId (accountId fromAccount) (accountId toAccount) amount 0
        recordTransaction fromAccount transaction
        recordTransaction toAccount transaction
    return success
```

#### ParallelSort.hs (병렬 정렬)
```haskell
module ParallelSort where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List

-- 병렬 머지 정렬
parallelMergeSort :: Ord a => [a] -> [a]
parallelMergeSort [] = []
parallelMergeSort [x] = [x]
parallelMergeSort xs = 
    let (left, right) = splitAt (length xs `div` 2) xs
        sortedLeft = parallelMergeSort left
        sortedRight = parallelMergeSort right
    in sortedLeft `par` sortedRight `pseq` merge sortedLeft sortedRight

-- 머지 함수
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- 병렬 퀵 정렬
parallelQuickSort :: Ord a => [a] -> [a]
parallelQuickSort [] = []
parallelQuickSort [x] = [x]
parallelQuickSort (x:xs) = 
    let (smaller, larger) = partition (<= x) xs
        sortedSmaller = parallelQuickSort smaller
        sortedLarger = parallelQuickSort larger
    in sortedSmaller `par` sortedLarger `pseq` sortedSmaller ++ [x] ++ sortedLarger

-- 전략을 사용한 병렬 처리
parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f xs = map f xs `using` parList rseq

-- 병렬 필터
parallelFilter :: (a -> Bool) -> [a] -> [a]
parallelFilter p xs = filter p xs `using` parList rseq

-- 병렬 폴드
parallelFold :: (a -> a -> a) -> a -> [a] -> a
parallelFold f z xs = foldl f z xs `using` parList rseq

-- 병렬 행렬 곱셈
type Matrix = [[Double]]

parallelMatrixMultiply :: Matrix -> Matrix -> Matrix
parallelMatrixMultiply a b = 
    let result = [[sum [a !! i !! k * b !! k !! j | k <- [0..length (head b) - 1]] 
                   | j <- [0..length (head b) - 1]] 
                  | i <- [0..length a - 1]]
    in result `using` parList (parList rseq)
```

#### WebServer.hs (멀티스레드 웹 서버)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module WebServer where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- HTTP 요청 타입
data HttpRequest = HttpRequest
    { method :: Text
    , path :: Text
    , headers :: [(Text, Text)]
    , body :: Text
    }

-- HTTP 응답 타입
data HttpResponse = HttpResponse
    { status :: Int
    , responseHeaders :: [(Text, Text)]
    , responseBody :: Text
    }

-- 웹 서버 설정
data ServerConfig = ServerConfig
    { port :: Int
    , maxConnections :: Int
    , timeout :: Int
    }

-- 기본 설정
defaultConfig :: ServerConfig
defaultConfig = ServerConfig 8080 100 30

-- 웹 서버 시작
startServer :: ServerConfig -> IO ()
startServer config = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral (port config)) iNADDR_ANY)
    listen sock (maxConnections config)
    putStrLn $ "서버가 포트 " ++ show (port config) ++ "에서 시작되었습니다."
    acceptLoop sock config

-- 연결 수락 루프
acceptLoop :: Socket -> ServerConfig -> IO ()
acceptLoop sock config = do
    (conn, addr) <- accept sock
    putStrLn $ "새 연결: " ++ show addr
    -- 비동기로 연결 처리
    _ <- async $ handleConnection conn config
    acceptLoop sock config

-- 연결 처리
handleConnection :: Socket -> ServerConfig -> IO ()
handleConnection conn config = do
    request <- receiveRequest conn
    response <- processRequest request
    sendResponse conn response
    close conn

-- HTTP 요청 수신
receiveRequest :: Socket -> IO HttpRequest
receiveRequest conn = do
    data <- recv conn 4096
    let requestText = TE.decodeUtf8 data
    return $ parseRequest requestText

-- HTTP 요청 파싱
parseRequest :: Text -> HttpRequest
parseRequest requestText = 
    let lines = T.lines requestText
        requestLine = head lines
        parts = T.words requestLine
        method = head parts
        path = parts !! 1
        headers = parseHeaders (tail lines)
        body = T.unlines (dropWhile (/= "") (tail lines))
    in HttpRequest method path headers body

-- 헤더 파싱
parseHeaders :: [Text] -> [(Text, Text)]
parseHeaders [] = []
parseHeaders (line:rest)
    | T.null line = []
    | otherwise = 
        let (name, value) = T.breakOn ":" line
        in (T.strip name, T.strip (T.drop 1 value)) : parseHeaders rest

-- 요청 처리
processRequest :: HttpRequest -> IO HttpResponse
processRequest request = do
    case path request of
        "/" -> return $ HttpResponse 200 [("Content-Type", "text/html")] 
               "<html><body><h1>Hello, Haskell Web Server!</h1></body></html>"
        "/api/status" -> return $ HttpResponse 200 [("Content-Type", "application/json")]
                        "{\"status\": \"ok\", \"message\": \"Server is running\"}"
        _ -> return $ HttpResponse 404 [("Content-Type", "text/html")]
             "<html><body><h1>404 Not Found</h1></body></html>"

-- HTTP 응답 전송
sendResponse :: Socket -> HttpResponse -> IO ()
sendResponse conn response = do
    let statusLine = "HTTP/1.1 " ++ show (status response) ++ " OK\r\n"
        headers = T.unlines [name ++ ": " ++ value | (name, value) <- responseHeaders response]
        responseText = statusLine ++ T.unpack headers ++ "\r\n" ++ T.unpack (responseBody response)
    sendAll conn (C8.pack responseText)

-- 메인 함수
main :: IO ()
main = startServer defaultConfig
```

#### ChatServer.hs (실시간 채팅 서버)
```haskell
module ChatServer where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network.Socket
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- 채팅 메시지 타입
data ChatMessage = ChatMessage
    { sender :: Text
    , content :: Text
    , timestamp :: Int
    }

-- 채팅 룸 타입
data ChatRoom = ChatRoom
    { roomName :: Text
    , clients :: TVar [Client]
    , messages :: TVar [ChatMessage]
    }

-- 클라이언트 타입
data Client = Client
    { clientSocket :: Socket
    , clientName :: Text
    , clientRoom :: TVar ChatRoom
    }

-- 채팅 서버 시작
startChatServer :: Int -> IO ()
startChatServer port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
    listen sock 10
    putStrLn $ "채팅 서버가 포트 " ++ show port ++ "에서 시작되었습니다."
    
    -- 기본 룸 생성
    defaultRoom <- atomically $ createRoom "general"
    
    -- 클라이언트 연결 수락 루프
    acceptClients sock defaultRoom

-- 채팅 룸 생성
createRoom :: Text -> STM ChatRoom
createRoom name = do
    clientsVar <- newTVar []
    messagesVar <- newTVar []
    return $ ChatRoom name clientsVar messagesVar

-- 클라이언트 연결 수락
acceptClients :: Socket -> ChatRoom -> IO ()
acceptClients sock room = do
    (conn, addr) <- accept sock
    putStrLn $ "새 클라이언트 연결: " ++ show addr
    _ <- forkIO $ handleClient conn room
    acceptClients sock room

-- 클라이언트 처리
handleClient :: Socket -> ChatRoom -> IO ()
handleClient conn room = do
    -- 클라이언트 이름 요청
    sendMessage conn "이름을 입력하세요: "
    nameData <- recv conn 1024
    let name = TE.decodeUtf8 nameData
    let client = Client conn (T.strip name) (newTVarIO room)
    
    -- 룸에 클라이언트 추가
    atomically $ addClient room client
    
    -- 환영 메시지 전송
    sendMessage conn $ "환영합니다, " ++ T.unpack (T.strip name) ++ "님!"
    
    -- 메시지 수신 루프
    messageLoop client

-- 클라이언트를 룸에 추가
addClient :: ChatRoom -> Client -> STM ()
addClient room client = do
    clients <- readTVar (clients room)
    writeTVar (clients room) (client : clients)

-- 메시지 루프
messageLoop :: Client -> IO ()
messageLoop client = do
    data <- recv (clientSocket client) 1024
    if BS.null data
        then do
            -- 클라이언트 연결 종료
            room <- readTVarIO (clientRoom client)
            atomically $ removeClient room client
            close (clientSocket client)
        else do
            let message = TE.decodeUtf8 data
            room <- readTVarIO (clientRoom client)
            let chatMessage = ChatMessage (clientName client) (T.strip message) 0
            atomically $ addMessage room chatMessage
            broadcastMessage room chatMessage
            messageLoop client

-- 클라이언트 제거
removeClient :: ChatRoom -> Client -> STM ()
removeClient room client = do
    clients <- readTVar (clients room)
    writeTVar (clients room) (filter (/= client) clients)

-- 메시지 추가
addMessage :: ChatRoom -> ChatMessage -> STM ()
addMessage room message = do
    messages <- readTVar (messages room)
    writeTVar (messages room) (message : messages)

-- 메시지 브로드캐스트
broadcastMessage :: ChatRoom -> ChatMessage -> IO ()
broadcastMessage room message = do
    clients <- atomically $ readTVar (clients room)
    let messageText = T.unpack (sender message) ++ ": " ++ T.unpack (content message)
    mapM_ (\client -> sendMessage (clientSocket client) messageText) clients

-- 메시지 전송
sendMessage :: Socket -> String -> IO ()
sendMessage conn message = do
    sendAll conn (C8.pack (message ++ "\n"))

-- 메인 함수
main :: IO ()
main = startChatServer 8080
```

## 연습 문제
1. STM을 사용한 생산자-소비자 패턴을 구현하세요
2. 병렬 피보나치 수열 계산기를 만들어보세요
3. 멀티스레드 파일 다운로드 매니저를 구현하세요
4. 실시간 주식 가격 모니터링 시스템을 만들어보세요

## 고급 연습 문제
1. 분산 시스템 시뮬레이터를 구현하세요
2. 병렬 머신 러닝 알고리즘을 구현하세요
3. 고성능 웹 프록시 서버를 만들어보세요

## 테스트 방법
```bash
# 웹 서버 실행
ghc -threaded WebServer.hs
./WebServer

# 채팅 서버 실행
ghc -threaded ChatServer.hs
./ChatServer

# 병렬 정렬 테스트
ghc -threaded -O2 ParallelSort.hs
./ParallelSort
```

## 다음 단계
9단계에서는 웹 개발과 실무 프로젝트에 대해 학습합니다.
