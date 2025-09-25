module Lib
    ( someFunc
    , simpleWebServer
    , handleRequest
    , parseRequest
    , createResponse
    ) where

import Network.Socket
import qualified Data.ByteString.Char8 as C8
import Data.Char

-- 간단한 웹 서버 시뮬레이션
simpleWebServer :: IO ()
simpleWebServer = do
    putStrLn "=== 간단한 웹 서버 시뮬레이션 ==="
    putStrLn "서버가 포트 8080에서 시작되었습니다."
    putStrLn "요청 처리 시뮬레이션:"
    
    -- 시뮬레이션된 요청들
    let requests = [
            "GET / HTTP/1.1\r\nHost: localhost:8080\r\n\r\n",
            "GET /api/status HTTP/1.1\r\nHost: localhost:8080\r\n\r\n",
            "GET /api/users HTTP/1.1\r\nHost: localhost:8080\r\n\r\n"
        ]
    
    mapM_ handleRequest requests

-- 요청 처리
handleRequest :: String -> IO ()
handleRequest request = do
    putStrLn $ "\n받은 요청:"
    putStrLn request
    let response = createResponse request
    putStrLn "응답:"
    putStrLn response

-- 요청 파싱
parseRequest :: String -> (String, String)
parseRequest request = 
    let lines = lines request
        requestLine = head lines
        parts = words requestLine
        method = head parts
        path = parts !! 1
    in (method, path)

-- 응답 생성
createResponse :: String -> String
createResponse request = 
    let (method, path) = parseRequest request
        body = case path of
            "/" -> "<html><body><h1>Hello, Haskell Web Server!</h1></body></html>"
            "/api/status" -> "{\"status\": \"ok\", \"message\": \"Server is running\"}"
            "/api/users" -> "[{\"id\": 1, \"name\": \"Alice\"}, {\"id\": 2, \"name\": \"Bob\"}]"
            _ -> "<html><body><h1>404 Not Found</h1></body></html>"
        status = if path == "/" || isInfixOf "/api/" path then "200 OK" else "404 Not Found"
    in "HTTP/1.1 " ++ status ++ "\r\n" ++
       "Content-Type: " ++ (if isInfixOf "/api/" path then "application/json" else "text/html") ++ "\r\n" ++
       "Content-Length: " ++ show (length body) ++ "\r\n" ++
       "\r\n" ++ body

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 웹 개발 예제 ==="
    putStrLn ""
    
    putStrLn "웹 서버 시뮬레이션을 시작합니다..."
    simpleWebServer
    
    putStrLn "\n=== REST API 예제 ==="
    putStrLn "API 엔드포인트:"
    putStrLn "GET / - 홈페이지"
    putStrLn "GET /api/status - 서버 상태"
    putStrLn "GET /api/users - 사용자 목록"
    putStrLn "POST /api/users - 사용자 생성"
    putStrLn "PUT /api/users/{id} - 사용자 수정"
    putStrLn "DELETE /api/users/{id} - 사용자 삭제"
