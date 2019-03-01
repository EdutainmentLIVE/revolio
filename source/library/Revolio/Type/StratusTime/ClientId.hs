module Revolio.Type.StratusTime.ClientId
  ( StratusTimeClientId
  , textToStratusTimeClientId
  , stratusTimeClientIdToText
  )
where

import qualified Data.Text as Text
import qualified Network.HTTP.Types.QueryLike as Http

newtype StratusTimeClientId = StratusTimeClientId
  { unwrapStratusTimeClientId :: Text.Text
  } deriving (Eq, Show)

instance Http.QueryValueLike StratusTimeClientId where
  toQueryValue = Http.toQueryValue . stratusTimeClientIdToText

textToStratusTimeClientId :: Text.Text -> StratusTimeClientId
textToStratusTimeClientId = StratusTimeClientId

stratusTimeClientIdToText :: StratusTimeClientId -> Text.Text
stratusTimeClientIdToText = unwrapStratusTimeClientId
