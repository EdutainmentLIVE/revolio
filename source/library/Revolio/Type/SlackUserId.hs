module Revolio.Type.SlackUserId
  ( SlackUserId
  , textToSlackUserId
  , slackUserIdToText
  )
where

import qualified Data.Text as Text

newtype SlackUserId
  = SlackUserId Text.Text
  deriving (Eq, Ord, Show)

textToSlackUserId :: Text.Text -> SlackUserId
textToSlackUserId = SlackUserId

slackUserIdToText :: SlackUserId -> Text.Text
slackUserIdToText (SlackUserId text) = text
