module Revolio.Type.SlackSigningSecret
  ( SlackSigningSecret
  , textToSlackSigningSecret
  , slackSigningSecretToText
  )
where

import qualified Data.ByteArray as Memory
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

newtype SlackSigningSecret = SlackSigningSecret
  { unwrapSlackSigningSecret :: Memory.ScrubbedBytes
  } deriving (Eq, Show)

instance Memory.ByteArrayAccess SlackSigningSecret where
  length = Memory.length . unwrapSlackSigningSecret
  withByteArray = Memory.withByteArray . unwrapSlackSigningSecret

textToSlackSigningSecret :: Text.Text -> SlackSigningSecret
textToSlackSigningSecret =
  SlackSigningSecret . Memory.convert . Encoding.encodeUtf8

slackSigningSecretToText :: SlackSigningSecret -> Text.Text
slackSigningSecretToText =
  Encoding.decodeUtf8 . Memory.convert . unwrapSlackSigningSecret
