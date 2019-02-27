module Revolio.Type.Command
  ( Command(..)
  , textToCommand
  , commandToText
  )
where

import qualified Data.Text as Text

data Command
  = CommandClock
  deriving (Eq, Show)

textToCommand :: Text.Text -> Either String Command
textToCommand text = case Text.unpack text of
  "/clock" -> Right CommandClock
  _ -> Left $ "unknown command: " <> show text

commandToText :: Command -> Text.Text
commandToText command = Text.pack $ case command of
  CommandClock -> "/clock"
