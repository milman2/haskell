module Step1.Structure where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import qualified Text.Megaparsec as MP

-- Parsec → ParsecT → MonadParsec
-- Megaparsec의 표준 타입 별칭: Parsec e s a
type Parser = Parsec Void Text

-- Text 기반 입력 처리 (Data.Text) + chunk 사용
pHello :: Parser Text
pHello = MP.chunk (T.pack "hello")

demo :: IO ()
demo = do
  putStrLn "=== Megaparsec 기본 구조 ==="
  print $ parse pHello "demo" (T.pack "hello")
  putStrLn "\n에러 메시지 (errorBundlePretty):"
  case parse pHello "demo" (T.pack "hey") of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right _     -> pure ()

main :: IO ()
main = demo

