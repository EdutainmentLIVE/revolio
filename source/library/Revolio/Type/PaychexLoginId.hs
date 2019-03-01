module Revolio.Type.PaychexLoginId
  ( PaychexLoginId
  , textToPaychexLoginId
  , paychexLoginIdToText
  )
where

import qualified Data.Text as Text
import qualified Network.HTTP.Types.QueryLike as Http

newtype PaychexLoginId = PaychexLoginId
  { unwrapPaychexLoginId :: Text.Text
  } deriving (Eq, Show)

instance Http.QueryValueLike PaychexLoginId where
  toQueryValue = Http.toQueryValue . paychexLoginIdToText

textToPaychexLoginId :: Text.Text -> PaychexLoginId
textToPaychexLoginId = PaychexLoginId

paychexLoginIdToText :: PaychexLoginId -> Text.Text
paychexLoginIdToText = unwrapPaychexLoginId
