{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module SimpleMath where

-- 간단한 수학 EDSL 예제
-- 이 EDSL은 수학 표현식을 정의하고 계산할 수 있게 해줍니다

-- 1. 수학 표현식의 데이터 타입 정의
data MathExpr
    = Number Double
    | Variable String
    | Add MathExpr MathExpr
    | Subtract MathExpr MathExpr
    | Multiply MathExpr MathExpr
    | Divide MathExpr MathExpr
    | Power MathExpr MathExpr
    | Sin MathExpr
    | Cos MathExpr
    | Log MathExpr
    deriving (Show, Eq)

-- 2. 변수 환경 (변수명 -> 값)
type Env = [(String, Double)]

-- 3. 수학 표현식 계산 함수
eval :: Env -> MathExpr -> Double
eval _ (Number n) = n
eval env (Variable name) = 
    case lookup name env of
        Just value -> value
        Nothing -> error $ "Variable not found: " ++ name
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Subtract e1 e2) = eval env e1 - eval env e2
eval env (Multiply e1 e2) = eval env e1 * eval env e2
eval env (Divide e1 e2) = eval env e1 / eval env e2
eval env (Power e1 e2) = eval env e1 ** eval env e2
eval env (Sin e) = sin (eval env e)
eval env (Cos e) = cos (eval env e)
eval env (Log e) = log (eval env e)

-- 4. EDSL을 위한 연산자 오버로딩
instance Num MathExpr where
    (+) = Add
    (-) = Subtract
    (*) = Multiply
    abs = error "abs not implemented for MathExpr"
    signum = error "signum not implemented for MathExpr"
    fromInteger = Number . fromInteger

instance Fractional MathExpr where
    (/) = Divide
    fromRational = Number . fromRational

instance Floating MathExpr where
    pi = Number pi
    exp = error "exp not implemented for MathExpr"
    log = Log
    sin = Sin
    cos = Cos
    asin = error "asin not implemented for MathExpr"
    acos = error "acos not implemented for MathExpr"
    atan = error "atan not implemented for MathExpr"
    sinh = error "sinh not implemented for MathExpr"
    cosh = error "cosh not implemented for MathExpr"
    asinh = error "asinh not implemented for MathExpr"
    acosh = error "acosh not implemented for MathExpr"
    atanh = error "atanh not implemented for MathExpr"

-- 5. 변수 생성 함수
var :: String -> MathExpr
var = Variable

-- 6. 표현식을 문자열로 변환하는 함수
showExpr :: MathExpr -> String
showExpr (Number n) = show n
showExpr (Variable name) = name
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Subtract e1 e2) = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Multiply e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Divide e1 e2) = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"
showExpr (Power e1 e2) = "(" ++ showExpr e1 ++ " ^ " ++ showExpr e2 ++ ")"
showExpr (Sin e) = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e) = "cos(" ++ showExpr e ++ ")"
showExpr (Log e) = "log(" ++ showExpr e ++ ")"

-- 7. 사용 예시
example1 :: MathExpr
example1 = (var "x" + 2) * (var "y" - 1)

example2 :: MathExpr
example2 = sin (var "theta") + cos (var "theta")

example3 :: MathExpr
example3 = log (var "x" * var "y") + var "z"

-- 8. 테스트 함수
testMath :: IO ()
testMath = do
    let env = [("x", 3.0), ("y", 4.0), ("theta", pi/4), ("z", 2.0)]
    
    putStrLn "=== Simple Math EDSL Examples ==="
    putStrLn ""
    
    putStrLn "Example 1: (x + 2) * (y - 1)"
    putStrLn $ "Expression: " ++ showExpr example1
    putStrLn $ "Result: " ++ show (eval env example1)
    putStrLn ""
    
    putStrLn "Example 2: sin(theta) + cos(theta)"
    putStrLn $ "Expression: " ++ showExpr example2
    putStrLn $ "Result: " ++ show (eval env example2)
    putStrLn ""
    
    putStrLn "Example 3: log(x * y) + z"
    putStrLn $ "Expression: " ++ showExpr example3
    putStrLn $ "Result: " ++ show (eval env example3)
    putStrLn ""
    
    putStrLn "Environment:"
    mapM_ (\(name, value) -> putStrLn $ "  " ++ name ++ " = " ++ show value) env
