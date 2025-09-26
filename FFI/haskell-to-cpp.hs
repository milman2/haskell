{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

-- C++ 함수 선언 (extern "C"로 래핑된 함수)
foreign import ccall "cpp_functions.h add_numbers" 
    c_addNumbers :: CInt -> CInt -> CInt

foreign import ccall "cpp_functions.h multiply_numbers" 
    c_multiplyNumbers :: CInt -> CInt -> CInt

foreign import ccall "cpp_functions.h get_greeting" 
    c_getGreeting :: IO CString

foreign import ccall "cpp_functions.h free_string" 
    c_freeString :: CString -> IO ()

-- Calculator 클래스 관련 함수들
foreign import ccall "cpp_functions.h create_calculator" 
    c_createCalculator :: CInt -> IO (Ptr ())

foreign import ccall "cpp_functions.h calculator_add" 
    c_calculatorAdd :: Ptr () -> CInt -> IO CInt

foreign import ccall "cpp_functions.h calculator_multiply" 
    c_calculatorMultiply :: Ptr () -> CInt -> IO CInt

foreign import ccall "cpp_functions.h calculator_get_value" 
    c_calculatorGetValue :: Ptr () -> IO CInt

foreign import ccall "cpp_functions.h calculator_reset" 
    c_calculatorReset :: Ptr () -> IO ()

foreign import ccall "cpp_functions.h destroy_calculator" 
    c_destroyCalculator :: Ptr () -> IO ()

-- Haskell 래퍼 함수들
addNumbers :: Int -> Int -> Int
addNumbers x y = fromIntegral $ c_addNumbers (fromIntegral x) (fromIntegral y)

multiplyNumbers :: Int -> Int -> Int
multiplyNumbers x y = fromIntegral $ c_multiplyNumbers (fromIntegral x) (fromIntegral y)

getGreeting :: IO String
getGreeting = do
    cStr <- c_getGreeting
    str <- peekCString cStr
    c_freeString cStr
    return str

-- Calculator 래퍼 함수들
createCalculator :: Int -> IO (Ptr ())
createCalculator initial = c_createCalculator (fromIntegral initial)

calculatorAdd :: Ptr () -> Int -> IO Int
calculatorAdd calc n = fromIntegral <$> c_calculatorAdd calc (fromIntegral n)

calculatorMultiply :: Ptr () -> Int -> IO Int
calculatorMultiply calc n = fromIntegral <$> c_calculatorMultiply calc (fromIntegral n)

calculatorGetValue :: Ptr () -> IO Int
calculatorGetValue calc = fromIntegral <$> c_calculatorGetValue calc

calculatorReset :: Ptr () -> IO ()
calculatorReset = c_calculatorReset

destroyCalculator :: Ptr () -> IO ()
destroyCalculator = c_destroyCalculator

-- 테스트 함수
testCppFunctions :: IO ()
testCppFunctions = do
    putStrLn "=== Haskell에서 C++ 함수 호출 테스트 ==="
    
    -- 기본 함수 테스트
    let sum = addNumbers 10 20
    putStrLn $ "10 + 20 = " ++ show sum
    
    let product = multiplyNumbers 5 6
    putStrLn $ "5 * 6 = " ++ show product
    
    greeting <- getGreeting
    putStrLn $ "C++에서 온 인사: " ++ greeting
    
    putStrLn ""
    putStrLn "=== Calculator 클래스 테스트 ==="
    
    -- Calculator 생성 (초기값 10)
    calc <- createCalculator 10
    putStrLn "Calculator 생성 (초기값: 10)"
    
    -- 현재 값 확인
    currentValue <- calculatorGetValue calc
    putStrLn $ "현재 값: " ++ show currentValue
    
    -- 5 더하기
    newValue <- calculatorAdd calc 5
    putStrLn $ "10 + 5 = " ++ show newValue
    
    -- 3 곱하기
    multipliedValue <- calculatorMultiply calc 3
    putStrLn $ "15 * 3 = " ++ show multipliedValue
    
    -- 최종 값 확인
    finalValue <- calculatorGetValue calc
    putStrLn $ "최종 값: " ++ show finalValue
    
    -- 리셋
    calculatorReset calc
    resetValue <- calculatorGetValue calc
    putStrLn $ "리셋 후 값: " ++ show resetValue
    
    -- Calculator 정리
    destroyCalculator calc
    putStrLn "Calculator 정리 완료"

-- main 함수 추가
main :: IO ()
main = testCppFunctions
