module Main where

import BasicGADT
import DSLGADT
import AdvancedGADT
import ConfigGADT

main :: IO ()
main = do
    testBasicGADT
    putStrLn $ "=" ++ replicate 50 '='
    putStrLn ""
    
    testDSLGADT
    putStrLn $ "=" ++ replicate 50 '='
    putStrLn ""
    
    testAdvancedGADT
    putStrLn $ "=" ++ replicate 50 '='
    putStrLn ""
    
    testConfigGADT
    putStrLn $ "=" ++ replicate 50 '='
    putStrLn ""
    
    putStrLn "=== GADT 학습 요약 ==="
    putStrLn ""
    putStrLn "1. 기본 GADT:"
    putStrLn "   - 타입 안전한 표현식"
    putStrLn "   - 패턴 매칭 시 타입 정보 보존"
    putStrLn "   - 잘못된 타입 조합 방지"
    putStrLn ""
    putStrLn "2. DSL에서의 GADT:"
    putStrLn "   - 타입 안전한 SQL 쿼리"
    putStrLn "   - 쿼리 타입에 따른 결과 타입 자동 결정"
    putStrLn "   - 컴파일 타임 오류 검출"
    putStrLn ""
    putStrLn "3. 고급 GADT:"
    putStrLn "   - 타입 레벨 프로그래밍"
    putStrLn "   - 벡터와 행렬의 크기 정보 보존"
    putStrLn "   - 컴파일 타임 크기 검증"
    putStrLn ""
    putStrLn "4. 설정 DSL에서의 GADT:"
    putStrLn "   - 타입 안전한 설정 관리"
    putStrLn "   - 중첩된 설정 구조의 타입 안전성"
    putStrLn "   - 설정 검증의 타입 안전성"
    putStrLn ""
    putStrLn "GADT는 Haskell에서 강력한 타입 안전성을 제공하며,"
    putStrLn "DSL 구현 시 특히 유용합니다!"
