module Revolio.Type.StratusTime.LoginId
  ( StratusTimeLoginId
  , textToStratusTimeLoginId
  , stratusTimeLoginIdToText
  )
where

import qualified Data.Text as Text
import qualified Network.HTTP.Types.QueryLike as Http

newtype StratusTimeLoginId = StratusTimeLoginId
  { unwrapStratusTimeLoginId :: Text.Text
  } deriving (Eq, Show)

instance Http.QueryValueLike StratusTimeLoginId where
  toQueryValue = Http.toQueryValue . stratusTimeLoginIdToText

textToStratusTimeLoginId :: Text.Text -> StratusTimeLoginId
textToStratusTimeLoginId = StratusTimeLoginId

stratusTimeLoginIdToText :: StratusTimeLoginId -> Text.Text
stratusTimeLoginIdToText = unwrapStratusTimeLoginId
