module Main where

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser x) = Parser $ \s -> do
        (x', s') <- x s
        return (f x', s')
instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    (Parser f) <*> (Parser x) = Parser $ \s -> do
        (f', s1) <- f s
        (x', s2) <- x s1
        return (f' x', s2)

instance Monad Parser where
    (Parser x) >>= f = Parser $ \s -> do
        (x', s') <- x s
        runParser (f x') s'

instance MonadFail Parser where
    fail s = Parser $ \_ -> Nothing

class (Applicative f) => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    some :: f a -> f [a]
    some v = some_v
        where many_v = some_v <|> pure []
              some_v = (:) <$> v <*> many_v
    many :: f a -> f [a]
    many v = many_v
        where many_v = some_v <|> pure []
              some_v = (:) <$> v <*> many_v

              

instance Alternative Parser where
    empty = fail ""
    (Parser x) <|> (Parser y) = Parser $ \s -> 
        case x s of
            Just x -> Just x
            Nothing -> y s

char :: Char -> Parser Char
char c = Parser charP
    where charP [] = Nothing
          charP (x:xs) | x == c = Just (x, xs) 
                       | otherwise = Nothing

string :: String -> Parser String
string = mapM char
{-
mapM f [] = []
mapM f (x:xs) = do
    x' <- f x
    xs' <- mapM f xs
    return (x' : xs')
-}

-- skip spaces
space :: Parser Char
space = char ' ' <|> char '\t' <|> char '\n' <|> char '\r'

ss = many space

parseHW = (,) <$> (string "Hello" <* ss) <*> string "World"

parseIntChar :: Parser Char
parseIntChar = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'

parseInt :: Parser Int
parseInt = str2Int 0 <$> some parseIntChar 

str2Int :: Int -> String -> Int
str2Int acc []     = acc
str2Int acc (x:xs) = str2Int (acc * 10 + charToInt x) xs

charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

main :: IO ()
main = do
    putStrLn "Hello, World!"






