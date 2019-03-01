module Revolio.Type.Action
  ( Action(..)
  , textToAction
  , actionToText
  )
where

import qualified Data.Text as Text
import qualified Revolio.Type.Direction as Type
import qualified Revolio.Type.PaychexLoginId as Type
import qualified Revolio.Type.PaychexPassword as Type

data Action
  = ActionClock Type.Direction
  | ActionHelp
  | ActionSetup Type.PaychexLoginId Type.PaychexPassword
  deriving (Eq, Show)

textToAction :: Text.Text -> Either String Action
textToAction text = case Text.unpack <$> Text.words text of
  ["clock", "in"] -> Right $ ActionClock Type.DirectionIn
  ["clock", "out"] -> Right $ ActionClock Type.DirectionOut
  ["help"] -> Right ActionHelp
  ["setup", username, password] -> Right $ ActionSetup
    (Type.textToPaychexLoginId $ Text.pack username)
    (Type.textToPaychexPassword $ Text.pack password)
  _ -> Left $ "unknown action: " <> show text

actionToText :: Action -> Text.Text
actionToText action = case action of
  ActionClock direction -> Text.pack $ "clock " <> case direction of
    Type.DirectionIn -> "in"
    Type.DirectionOut -> "out"
  ActionHelp -> Text.pack "help"
  ActionSetup username password -> Text.unwords
    [ Text.pack "setup"
    , Type.paychexLoginIdToText username
    , Type.paychexPasswordToText password
    ]
