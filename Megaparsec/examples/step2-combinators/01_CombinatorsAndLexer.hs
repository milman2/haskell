module Step2.CombinatorsAndLexer where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, errorBundlePretty, (<|>), many, some, between)
import Text.Megaparsec.Char (char, space1, letterChar, digitChar)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative.Combinators as AC

type Parser = Parsec Void Text

-- Lexer helpers
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment (T.pack "--")) (L.skipBlockComment (T.pack "{-") (T.pack "-}"))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

ident :: Parser Text
ident = lexeme $ do
  h <- letterChar
  t <- many (letterChar <|> digitChar)
  pure (T.pack (h:t))

integer :: Parser Integer
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol (T.pack "(")) (symbol (T.pack ")"))

commaSep :: Parser a -> Parser [a]
commaSep p = p `AC.sepBy` symbol (T.pack ",")

demo :: IO ()
demo = do
  putStrLn "=== Combinators & Lexer ==="
  print $ parse (some letterChar :: Parser String) "demo" (T.pack "abc")
  print $ parse (many letterChar :: Parser String) "demo" (T.pack "")
  print $ parse (ident) "demo" (T.pack "name123")
  print $ parse (integer) "demo" (T.pack "42")
  print $ parse (parens (commaSep ident)) "demo" (T.pack "(a,b,c)")
  case parse (symbol (T.pack "+") <|> symbol (T.pack "-")) "demo" (T.pack "*") of
    Left e  -> putStrLn (errorBundlePretty e)
    Right t -> print t

main :: IO ()
main = demo

