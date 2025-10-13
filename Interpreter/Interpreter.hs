module Main where
import Foreign (withArray)

type Ident = String

data Expr = Number Int
    | Boolean Bool
    | Plus Expr Expr
    | Minus Expr Expr
    | If Expr Expr Expr
    | Equals Expr Expr
    | Var Ident
    | Let Defn Expr
    | Lam [Ident] Expr
    | Apply Expr [Expr]
    deriving (Show, Eq)


-- 
data Value = NumVal Int | BoolVal Bool | Closure [Ident] Expr Env
    deriving (Show, Eq)

data Defn = Val Ident Expr
    | Rec Ident Expr
    deriving (Show, Eq)

type Env = [(Ident, Value)]


eval :: Expr -> Env -> Value
eval (Number i)    env = NumVal i
eval (Boolean b)   env = BoolVal b
eval (Equals e1 e2) env = BoolVal $ (eval e1 env) == (eval e2 env)
eval (Plus e1 e2)  env = let (NumVal n1) = eval e1 env in
                         let (NumVal n2) = eval e2 env in
                         NumVal (n1 + n2)
eval (Minus e1 e2) env = let (NumVal n1) = eval e1 env in
                         let (NumVal n2) = eval e2 env in
                         NumVal (n1 - n2)
eval (Var i)       env = find env i
eval (Let d e) env = eval e (elab env d)
eval (Lam ids e)   env = Closure ids e env
eval (If g e1 e2)  env = case eval g env of
    (BoolVal True) -> eval e1 env
    (BoolVal False) -> eval e2 env
eval (Apply f xs)  env = apply f' xs'
    where f' = eval f env
          xs' = map (flip eval env) xs

apply :: Value -> [Value] -> Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _ = error "Using a value as if it's a function"

find env i = snd $ head $ filter (\(i', _) -> i' == i) env
elab env (Val i e) = (i, eval e env) : env
elab env (Rec i (Lam args e)) = env' where env' = (i, Closure args e env') : env
elab _ _ = error "Only lambdas can be recursive"

main :: IO ()
main = do
    let expr = Plus (Number 1) (Number 2)
    print (eval expr [])

    let expr2 = Let (Val "x" $ Number 1) (Var "x")
    print (eval expr2 [])

    let expr3 = Minus (Number 22) (Var "x")
    print (eval expr3 [("x", NumVal 1)])

    let e = Let (Val "add" (Lam ["x", "y"] (Plus (Var "x") (Var "y")))) (Apply (Var "add") [Number 1, Number 2])
    print (eval e [])
    let expr4 = Let (Rec "sum" (Lam ["n"] (If (Equals (Var "n") (Number 0)) (Number 0) (Plus (Var "n") (Apply (Var "sum") [Minus (Var "n") (Number 1)]))))) 
    let ex = expr4 (Apply (Var "sum") [Number 5])
    print (eval ex [])