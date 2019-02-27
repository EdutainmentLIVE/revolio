module Revolio.Type.PaychexClientId
  ( PaychexClientId
  , textToPaychexClientId
  , paychexClientIdToText
  )
where

import qualified Data.Text as Text
import qualified Network.HTTP.Types.QueryLike as Http

newtype PaychexClientId
  = PaychexClientId Text.Text
  deriving (Eq, Show)

instance Http.QueryValueLike PaychexClientId where
  toQueryValue = Http.toQueryValue . paychexClientIdToText

textToPaychexClientId :: Text.Text -> PaychexClientId
textToPaychexClientId = PaychexClientId

paychexClientIdToText :: PaychexClientId -> Text.Text
paychexClientIdToText (PaychexClientId text) = text
