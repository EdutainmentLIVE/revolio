module Revolio.Type.PaychexPassword
  ( PaychexPassword
  , textToPaychexPassword
  , paychexPasswordToText
  )
where

import qualified Data.Text as Text
import qualified Network.HTTP.Types.QueryLike as Http

newtype PaychexPassword
  = PaychexPassword Text.Text
  deriving (Eq, Show)

instance Http.QueryValueLike PaychexPassword where
  toQueryValue = Http.toQueryValue . paychexPasswordToText

textToPaychexPassword :: Text.Text -> PaychexPassword
textToPaychexPassword = PaychexPassword

paychexPasswordToText :: PaychexPassword -> Text.Text
paychexPasswordToText (PaychexPassword text) = text
