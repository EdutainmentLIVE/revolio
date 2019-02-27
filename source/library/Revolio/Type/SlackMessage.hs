module Revolio.Type.SlackMessage
  ( SlackMessage
  , textToSlackMessage
  , slackMessageToText
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

newtype SlackMessage
  = SlackMessage Text.Text
  deriving (Eq, Show)

instance Aeson.ToJSON SlackMessage where
  toJSON slackMessage = Aeson.object
    [ Text.pack "response_type" Aeson..= "ephemeral"
    , Text.pack "text" Aeson..= slackMessageToText slackMessage
    ]

textToSlackMessage :: Text.Text -> SlackMessage
textToSlackMessage = SlackMessage

slackMessageToText :: SlackMessage -> Text.Text
slackMessageToText (SlackMessage text) = text
