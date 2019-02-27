module Revolio.Type.PaychexClientId
  ( PaychexClientId
  , textToPaychexClientId
  , paychexClientIdToText
  )
where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Types.QueryLike as Http

newtype PaychexClientId
  = PaychexClientId Text.Text
  deriving (Eq, Show)

instance Http.QueryValueLike PaychexClientId where
  toQueryValue = Just . Encoding.encodeUtf8 . paychexClientIdToText

textToPaychexClientId :: Text.Text -> PaychexClientId
textToPaychexClientId = PaychexClientId

paychexClientIdToText :: PaychexClientId -> Text.Text
paychexClientIdToText (PaychexClientId text) = text
