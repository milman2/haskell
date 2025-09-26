import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

data Person = Person {
  name :: Text ,
  age :: Int32 ,
} deriving (Show, Eq, Generic)
