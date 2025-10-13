{- LANGUAGE FlexibleInstances -}

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

-- instance MonadFail (Either String) where
--     fail = Left

data State m a = St { runState :: m -> (a, m) }

instance Functor (State m) where
    fmap f (St x) = St $ \m -> let (a, m') = x m in (f a, m')

instance Applicative (State m) where
    pure x = St $ \m -> (x, m)
    (St f) <*> (St x) = St $ \m -> let (f', m') = f m in
                                   let (x', m'') = x m' in (f' x', m'')

instance Monad (State m) where
    return = pure
    (St xm) >>= f = St $ \m -> let (x, m') = xm m in
                               let (St g) = f x in
                               g m'

get :: State m m
get = St $ \m -> (m, m)

set :: a -> State a ()
set x = St $ \_ -> ((), x)

type Mem = [Value]

type M a = State Mem a

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
    | New
    | Deref Expr
    | Seq Expr Expr
    | Assign Expr Expr
    deriving (Show, Eq)


-- 
data Value = NumVal Int | BoolVal Bool | Closure [Ident] Expr Env | Null | MemAddr Int
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
    ~(NumVal n1) <- eval e1 env 
    ~(NumVal n2) <- eval e2 env
    return $ NumVal (n1 + n2)
eval (Minus e1 e2)  env = do
    ~(NumVal n1) <- eval e1 env 
    ~(NumVal n2) <- eval e2 env
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

eval (New)          env = do
    mem <- get
    let ret =  length mem
    set $ mem ++ [Null]
    return $ MemAddr ret
eval (Deref e)      env = do
    ~(MemAddr i) <- eval e env
    mem <- get
    return $ mem !! i          
eval (Seq e1 e2)   env = eval e1 env >> eval e2 env
eval (Assign e1 e2) env = do
    ~(MemAddr i) <- eval e1 env
    e2' <- eval e2 env
    mem <- get
    let mem' = take i mem ++ [e2'] ++ drop (i+1) mem
    set mem'
    return e2' -- return Null


apply :: Value -> [Value] -> M Value
apply (Closure ids e env) vals = eval e (zip ids vals ++ env)
apply _ _ = error "Using a value as if it's a function"

find env i = snd $ head $ filter (\(i', _) -> i' == i) env

elab env (Val i e) = eval e env >>= \e' -> return $ (i, e'):env
elab env (Rec i l@(Lam ids e)) = return env' 
    where env' = (i, Closure ids e env'):env
elab _ _ = error "Only lambdas can be recursive"

main :: IO ()
main = do
    let expr1 = Let (Val "foo" (Number 42)) (Var "foo")
    let res = eval expr1 []
    print (runState res [])

    let expr2 = Let ((Val "x") New) (Seq (Assign (Var "x") (Number 42)) (Assign (Var "x") (Plus (Deref (Var "x")) (Number 1))))
    let res2 = eval expr2 []
    print (runState res2 [])

