module Revolio.Type.Command
  ( Command(..)
  , textToCommand
  , commandToText
  )
where

import qualified Data.Text as Text

data Command
  = CommandRevolio
  deriving (Eq, Show)

textToCommand :: Text.Text -> Either String Command
textToCommand text = case Text.unpack text of
  "/revolio" -> Right CommandRevolio
  _ -> Left $ "unknown command: " <> show text

commandToText :: Command -> Text.Text
commandToText command = Text.pack $ case command of
  CommandRevolio -> "/revolio"
