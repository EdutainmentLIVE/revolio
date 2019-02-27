module Revolio.Type.Action
  ( Action(..)
  , textToAction
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
  ["help"] -> Right ActionHelp
  ["in"] -> Right $ ActionClock Type.DirectionIn
  ["out"] -> Right $ ActionClock Type.DirectionOut
  ["setup", username, password] -> Right $ ActionSetup
    (Type.textToPaychexLoginId $ Text.pack username)
    (Type.textToPaychexPassword $ Text.pack password)
  _ -> Left $ "unknown action: " <> show text
