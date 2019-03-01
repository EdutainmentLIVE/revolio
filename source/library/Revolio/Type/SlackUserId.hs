module Revolio.Type.SlackUserId
  ( SlackUserId
  , textToSlackUserId
  , slackUserIdToText
  )
where

import qualified Data.Text as Text

newtype SlackUserId = SlackUserId
  { unwrapSlackUserId :: Text.Text
  } deriving (Eq, Ord, Show)

textToSlackUserId :: Text.Text -> SlackUserId
textToSlackUserId = SlackUserId

slackUserIdToText :: SlackUserId -> Text.Text
slackUserIdToText = unwrapSlackUserId
