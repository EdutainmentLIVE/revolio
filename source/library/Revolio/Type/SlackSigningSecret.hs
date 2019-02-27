module Revolio.Type.SlackSigningSecret
  ( SlackSigningSecret
  , textToSlackSigningSecret
  , slackSigningSecretToText
  )
where

import qualified Data.ByteArray as Memory
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

newtype SlackSigningSecret
  = SlackSigningSecret Text.Text
  deriving (Eq, Show)

instance Memory.ByteArrayAccess SlackSigningSecret where
  length = Memory.length . Encoding.encodeUtf8 . slackSigningSecretToText
  withByteArray =
    Memory.withByteArray . Encoding.encodeUtf8 . slackSigningSecretToText

textToSlackSigningSecret :: Text.Text -> SlackSigningSecret
textToSlackSigningSecret = SlackSigningSecret

slackSigningSecretToText :: SlackSigningSecret -> Text.Text
slackSigningSecretToText (SlackSigningSecret text) = text
