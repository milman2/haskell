module Lib
    ( someFunc
    , TodoItem(..)
    , TodoList
    , addTodo
    , completeTodo
    , deleteTodo
    , listTodos
    , todoApp
    ) where

import Data.Time
import Control.Monad.State

-- 할 일 관리 시스템
data TodoItem = TodoItem
    { todoId :: Int
    , todoTitle :: String
    , todoDescription :: String
    , todoCompleted :: Bool
    , todoCreatedAt :: UTCTime
    } deriving (Show, Eq)

type TodoList = [TodoItem]

-- 할 일 추가
addTodo :: String -> String -> State TodoList Int
addTodo title desc = do
    todos <- get
    let newId = if null todos then 1 else maximum (map todoId todos) + 1
    let newTodo = TodoItem newId title desc False (read "2024-01-01 00:00:00 UTC" :: UTCTime)
    put (newTodo : todos)
    return newId

-- 할 일 완료
completeTodo :: Int -> State TodoList Bool
completeTodo id = do
    todos <- get
    let updatedTodos = map (\todo -> if todoId todo == id then todo { todoCompleted = True } else todo) todos
    put updatedTodos
    return (any (\todo -> todoId todo == id) todos)

-- 할 일 삭제
deleteTodo :: Int -> State TodoList Bool
deleteTodo id = do
    todos <- get
    let filteredTodos = filter (\todo -> todoId todo /= id) todos
    put filteredTodos
    return (length todos /= length filteredTodos)

-- 할 일 목록 조회
listTodos :: State TodoList [TodoItem]
listTodos = get

-- 할 일 앱 시뮬레이션
todoApp :: IO ()
todoApp = do
    putStrLn "=== 할 일 관리 시스템 ==="
    putStrLn ""
    
    -- 초기 상태에서 할 일 추가
    let (id1, todos1) = runState (addTodo "Haskell 학습" "Haskell 언어를 체계적으로 학습하기") []
    let (id2, todos2) = runState (addTodo "프로젝트 완성" "최종 프로젝트 완성하기") todos1
    let (id3, todos3) = runState (addTodo "문서 작성" "학습 내용 정리하기") todos2
    
    putStrLn "할 일 추가 완료:"
    mapM_ print todos3
    putStrLn ""
    
    -- 할 일 완료
    let (_, todos4) = runState (completeTodo id1) todos3
    putStrLn "첫 번째 할 일 완료:"
    mapM_ print todos4
    putStrLn ""
    
    -- 할 일 삭제
    let (_, todos5) = runState (deleteTodo id2) todos4
    putStrLn "두 번째 할 일 삭제:"
    mapM_ print todos5
    putStrLn ""
    
    putStrLn "=== 최종 프로젝트 완성! ==="
    putStrLn "축하합니다! Haskell 학습 과정을 모두 완료했습니다."
    putStrLn "이제 실무에서 Haskell을 활용할 수 있습니다."

-- 메인 함수
someFunc :: IO ()
someFunc = do
    putStrLn "=== Haskell 최종 프로젝트 ==="
    putStrLn ""
    
    putStrLn "지금까지 학습한 내용:"
    putStrLn "1. 기본 문법과 타입 시스템"
    putStrLn "2. 리스트와 패턴 매칭"
    putStrLn "3. 재귀와 고차 함수"
    putStrLn "4. 타입 클래스와 모나드 기초"
    putStrLn "5. 입출력과 IO 모나드"
    putStrLn "6. 모듈과 패키지 관리"
    putStrLn "7. 고급 타입 시스템"
    putStrLn "8. 동시성과 병렬성"
    putStrLn "9. 웹 개발과 실무 프로젝트"
    putStrLn "10. 최종 프로젝트"
    putStrLn ""
    
    todoApp
