module Step3.ErrorsAndRecovery where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Maybe (catMaybes)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

pKey :: Parser Text
pKey = lexeme ((T.pack <$> some letterChar) <?> "key")

pVal :: Parser Text
pVal = lexeme ((T.pack <$> some (alphaNumChar <|> char '_')) <?> "value")

pPair :: Parser (Text,Text)
pPair = do
  k <- pKey
  _ <- symbol (T.pack "=") <?> "'='"
  v <- pVal
  pure (k,v)

pLine :: Parser (Maybe (Text,Text))
pLine = do
  -- 줄 끝까지 파싱 시도
  result <- withRecovery recover (Just <$> pPair)
  _ <- eol
  pure result
  where
    recover _ = do
      -- 빈 줄이거나 파싱 실패 시
      _ <- takeWhileP (Just "skip until newline") (/= '\n')
      pure Nothing

demo :: IO ()
demo = do
  putStrLn "=== Errors & Recovery ==="
  let input = T.pack "name=john\n\nage 25\ncity=seoul\n\n"
  case parse (many pLine) "kv" input of
    Left e  -> putStrLn (errorBundlePretty e)
    Right r -> print (catMaybes r)

main :: IO ()
main = demo

