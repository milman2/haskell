{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module BasicGADT where

-- 1. 기본 GADT 예제: 타입 안전한 표현식

-- 일반적인 ADT (비교용)
data ExprADT
    = LitADT Int
    | AddADT ExprADT ExprADT
    | BoolADT Bool
    deriving (Show, Eq)

-- GADT를 사용한 타입 안전한 표현식
data Expr a where
    LitInt  :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add     :: Expr Int -> Expr Int -> Expr Int
    Eq      :: Eq a => Expr a -> Expr a -> Expr Bool
    If      :: Expr Bool -> Expr a -> Expr a -> Expr a

-- 2. 타입 안전한 평가 함수
eval :: Expr a -> a
eval (LitInt n) = n
eval (LitBool b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Eq e1 e2) = eval e1 == eval e2
eval (If cond thenExpr elseExpr) = 
    if eval cond then eval thenExpr else eval elseExpr

-- 3. 타입 안전한 예제들
example1 :: Expr Int
example1 = Add (LitInt 5) (LitInt 3)

example2 :: Expr Bool
example2 = Eq (LitInt 5) (LitInt 3)

example3 :: Expr Int
example3 = If (LitBool True) (LitInt 10) (LitInt 20)

-- 4. 타입 안전성 검증
-- 이 코드는 컴파일되지 않습니다 (타입 오류):
-- badExample = Add (LitBool True) (LitInt 5)  -- Bool과 Int를 더할 수 없음

-- 5. 테스트 함수
testBasicGADT :: IO ()
testBasicGADT = do
    putStrLn "=== Basic GADT Examples ==="
    
    putStrLn $ "5 + 3 = " ++ show (eval example1)
    putStrLn $ "5 == 3 = " ++ show (eval example2)
    putStrLn $ "if True then 10 else 20 = " ++ show (eval example3)
    
    putStrLn "\nGADT의 장점:"
    putStrLn "- 타입 안전성: 잘못된 타입 조합은 컴파일 시점에 감지"
    putStrLn "- 패턴 매칭 시 타입 정보 보존"
    putStrLn "- 평가 함수에서 타입 캐스팅 불필요"
