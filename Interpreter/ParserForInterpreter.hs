{-# LANGUAGE FlexibleInstances, OverloadedStrings, ApplicativeDo #-}

module Main where
--import Foreign (withArray)

import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import qualified Data.Text as T

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

data Value = NumVal Int | BoolVal Bool | Closure [Ident] Expr Env | Null | MemAddr Int
    deriving (Show, Eq)

data Defn = Val Ident Expr
    | Rec Ident Expr
    deriving (Show, Eq)

parseFun t = parseOnly (skipSpace *> parseExpr <* skipSpace <* endOfInput) t

parseExpr :: Parser Expr
parseExpr = parsePM <|> parseExpr'

parseExpr' :: Parser Expr
parseExpr' = parseConst
        <|> parseIf
        <|> parseLet
        <|> parseLam
        <|> parseApp
        <|> parseVar

ss = skipSpace

atom = T.unpack <$> takeWhile1 (\c -> c /= ' ' && c /= '"' && c /= '-' && c /= '(' && c /= ')' && c /= '{' && c /= '}' && c /= '%' && c /= ':' && c /= '!')

parseConst :: Parser Expr
parseConst = Number <$>decimal 
        <|> string "True" *> pure (Boolean True)
        <|> string "False" *> pure (Boolean False)

parseIf :: Parser Expr
parseIf = do
    "if" *> ss    
    condition <- parseExpr <* ss
    "then" *> ss
    e1 <- parseExpr <* ss
    "else" *> ss
    e2 <- parseExpr <* ss
    return (If condition e1 e2)
    
parseVar = Var <$> atom

parseLet = do
    "let" *> ss
    d <- parseDefn <* ss
    "in" *> ss
    e <- parseExpr <* ss
    return (Let d e)

parseDefn =  Val <$> ("val" *> ss *> atom <* ss <* char '=' <* ss) <*> parseExpr
         <|> Rec <$> ("rec" *> ss *> atom <* ss <* char '=' <* ss) <*> parseExpr

parseLam = do
    char '!' -- \\ 대신 ! 사용.
    atoms <- many (atom <* ss)
    "->" *> ss
    expr <- parseExpr <* ss
    return $ Lam atoms expr

parsePM = parsePM' <|> parseTerm

parsePM' = do
    t <- parseTerm <* ss
    cons <- (char '+' *> pure Plus) <|> (char '-' *> pure Minus)
    ss
    e <- parseExpr
    return $ cons t e

parseTerm = parseTerm' <|> parseFactor
parseTerm' = do
    f <- parseFactor <* ss
    "==" *> ss
    t <- parseTerm
    return $ Equals f t

parseFactor = char '(' *> parseExpr <* char ')' 
           <|> parseExpr'

parseApp = do
    string "%{" *> ss
    f <- parseExpr <* ss
    char ':' *> ss
    ins <- many (parseExpr <* ss) 
    char '}'
    return $ Apply f ins


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

-- cabal install --lib attoparsec text
-- runghc ParserForInterpreter.hs
-- rlwrap ./tutorial8
-- let rec fib = !in -> if n == 0 then 0 else if n == 1 then 1 else %{fib:n-1} + %{fib:n-2} in %{fib:1}
-- let rec fib = !in -> if n == 0 then 0 else if n == 1 then 1 else %{fib:n-1} + %{fib:n-2} in %{fib:2}
-- let rec fib = !in -> if n == 0 then 0 else if n == 1 then 1 else %{fib:n-1} + %{fib:n-2} in %{fib:3}
-- let rec fib = !in -> if n == 0 then 0 else if n == 1 then 1 else %{fib:n-1} + %{fib:n-2} in %{fib:4}
main :: IO ()
main = do
    input <- getLine
    let result = parseFun (T.pack input)
    case result of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right expr -> do
            putStrLn $ "Parsed: " ++ show expr
            print $ runState (eval expr []) []

