module Revolio.Type.StratusTime.Password
  ( StratusTimePassword
  , textToStratusTimePassword
  , stratusTimePasswordToText
  )
where

import qualified Data.ByteArray as Memory
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Types.QueryLike as Http

newtype StratusTimePassword = StratusTimePassword
  { unwrapStratusTimePassword :: Memory.ScrubbedBytes
  } deriving (Eq, Show)

instance Http.QueryValueLike StratusTimePassword where
  toQueryValue = Http.toQueryValue . stratusTimePasswordToText

textToStratusTimePassword :: Text.Text -> StratusTimePassword
textToStratusTimePassword =
  StratusTimePassword . Memory.convert . Encoding.encodeUtf8

stratusTimePasswordToText :: StratusTimePassword -> Text.Text
stratusTimePasswordToText =
  Encoding.decodeUtf8 . Memory.convert . unwrapStratusTimePassword
