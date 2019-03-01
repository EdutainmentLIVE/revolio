module Revolio.Type.PaychexPassword
  ( PaychexPassword
  , textToPaychexPassword
  , paychexPasswordToText
  )
where

import qualified Data.ByteArray as Memory
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Types.QueryLike as Http

newtype PaychexPassword = PaychexPassword
  { unwrapPaychexPassword :: Memory.ScrubbedBytes
  } deriving (Eq, Show)

instance Http.QueryValueLike PaychexPassword where
  toQueryValue = Http.toQueryValue . paychexPasswordToText

textToPaychexPassword :: Text.Text -> PaychexPassword
textToPaychexPassword = PaychexPassword . Memory.convert . Encoding.encodeUtf8

paychexPasswordToText :: PaychexPassword -> Text.Text
paychexPasswordToText =
  Encoding.decodeUtf8 . Memory.convert . unwrapPaychexPassword
