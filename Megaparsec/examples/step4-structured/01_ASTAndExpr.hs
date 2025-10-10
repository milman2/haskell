module Step4.ASTAndExpr where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment (T.pack "--")) (L.skipBlockComment (T.pack "{-") (T.pack "-}"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

data Expr = ENum Integer | EVar Text | EAdd Expr Expr | ESub Expr Expr | EMul Expr Expr | EDiv Expr Expr deriving (Show, Eq)

parens :: Parser a -> Parser a
parens = between (symbol (T.pack "(")) (symbol (T.pack ")"))

ident :: Parser Text
ident = lexeme $ do h <- letterChar; t <- many (alphaNumChar <|> char '_'); pure (T.pack (h:t))

integer :: Parser Integer
integer = lexeme L.decimal

term :: Parser Expr
term = makeExprParser factor table
  where
    table =
      [ [ InfixL (EMul <$ symbol (T.pack "*"))
        , InfixL (EDiv <$ symbol (T.pack "/")) ]
      , [ InfixL (EAdd <$ symbol (T.pack "+"))
        , InfixL (ESub <$ symbol (T.pack "-")) ]
      ]
    factor = choice
      [ ENum <$> integer
      , EVar <$> ident
      , parens term
      ]

demo :: IO ()
demo = do
  putStrLn "=== AST & Expr ==="
  let samples = fmap T.pack ["1+2*3","(1+2)*3","x + 42","a*(b+c) - 5"]
  mapM_ (\s -> putStrLn ("input: " ++ T.unpack s) >> print (parse term "expr" s)) samples

main :: IO ()
main = demo

