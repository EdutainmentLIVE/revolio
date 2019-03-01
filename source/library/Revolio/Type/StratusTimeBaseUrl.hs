module Revolio.Type.StratusTimeBaseUrl
  ( StratusTimeBaseUrl
  , textToStratusTimeBaseUrl
  , stratusTimeBaseUrlToText
  )
where

import qualified Data.Text as Text

newtype StratusTimeBaseUrl = StratusTimeBaseUrl
  { unwrapStratusTimeBaseUrl :: Text.Text
  } deriving (Eq, Show)

textToStratusTimeBaseUrl :: Text.Text -> StratusTimeBaseUrl
textToStratusTimeBaseUrl = StratusTimeBaseUrl

stratusTimeBaseUrlToText :: StratusTimeBaseUrl -> Text.Text
stratusTimeBaseUrlToText = unwrapStratusTimeBaseUrl
