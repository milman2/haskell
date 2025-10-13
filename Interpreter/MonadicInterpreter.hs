{-# LANGUAGE FlexibleInstances #-}

module Main where
import Foreign (withArray)

type Ident = String

-- data Id a = Id a

-- instance Functor Id where
--     fmap f (Id x) = Id $ f x

-- instance Applicative Id where
--     (Id f) <*> (Id x) = Id $ f x
--     pure x = Id x

-- instance Monad Id where
--     (Id x) >>= f = f x

instance MonadFail (Either String) where
    fail = Left

-- type M a = Id a
type M a = Either String a

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


eval :: Expr -> Env -> M Value
eval (Number i)     env = return $ NumVal i
eval (Boolean b)    env = return $ BoolVal b
eval (Equals e1 e2) env = BoolVal <$> ((==) <$> (eval e1 env) <*> (eval e2 env))
eval (Plus e1 e2)   env = do
    (NumVal n1) <- eval e1 env 
    (NumVal n2) <- eval e2 env
    return $ NumVal (n1 + n2)
eval (Minus e1 e2)  env = do
    (NumVal n1) <- eval e1 env 
    (NumVal n2) <- eval e2 env
    return $ NumVal (n1 - n2)
eval (Var i)        env = return $ find env i
eval (Let d e)      env = elab env d >>= eval e
eval (Lam ids e)    env = return $ Closure ids e env
eval (If g e1 e2)   env = eval g env >>= \r -> case r of
    (BoolVal True) -> eval e1 env
    (BoolVal False) -> eval e2 env
eval (Apply f xs)   env = do
    f' <- eval f env
    xs' <- mapM (flip eval env) xs
    apply f' xs'
    where 
          

apply :: Value -> [Value] -> M Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _ = Left "Using a value as if it's a function"

find env i = snd $ head $ filter (\(i', _) -> i' == i) env

elab env (Val i e) = eval e env >>= \e' -> return $ (i, e'):env
elab env (Rec i l@(Lam ids e)) = return env' 
    where env' = (i, Closure ids e env'):env
elab _ _ = Left "Only lambdas can be recursive"

main :: IO ()
main = do
    let expr1 = Let (Rec "foo" (Number 42)) (Var "foo")
    print (eval expr1 [])

    let e = Let (Rec "sum" (Lam ["n"] (If (Equals (Var "n") (Number 0)) (Number 0) (Plus (Var "n") (Apply (Var "sum") [Minus (Var "n") (Number 1)]))))) 
    let result = eval (e (Apply (Var "sum") [Number 5])) []
    case result of
        Right val -> print val
        Left err -> putStrLn err