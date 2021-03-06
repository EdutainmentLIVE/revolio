module Revolio.Type.Action
  ( Action(..)
  , textToAction
  , actionToText
  )
where

import qualified Data.Text as Text
import qualified Revolio.Type.Direction as Type
import qualified Revolio.Type.StratusTime.Credentials as Type
import qualified Revolio.Type.StratusTime.LoginId as Type
import qualified Revolio.Type.StratusTime.Password as Type

data Action
  = ActionClock Type.Direction
  | ActionHelp
  | ActionSetup Type.StratusTimeCredentials
  deriving (Eq, Show)

textToAction :: Text.Text -> Either String Action
textToAction text = case Text.unpack <$> Text.words text of
  ["clock", direction] ->
    ActionClock <$> Type.textToDirection (Text.pack direction)
  ["help"] -> Right ActionHelp
  ["setup", username, password] -> Right $ ActionSetup
    Type.StratusTimeCredentials
      { Type.stratusTimeCredentialsLoginId = Type.textToStratusTimeLoginId
        $ Text.pack username
      , Type.stratusTimeCredentialsPassword = Type.textToStratusTimePassword
        $ Text.pack password
      }
  _ -> Left $ "unknown action: " <> show text

actionToText :: Action -> Text.Text
actionToText action = case action of
  ActionClock direction ->
    Text.unwords [Text.pack "clock", Type.directionToText direction]
  ActionHelp -> Text.pack "help"
  ActionSetup credentials -> Text.unwords
    [ Text.pack "setup"
    , Type.stratusTimeLoginIdToText
      $ Type.stratusTimeCredentialsLoginId credentials
    , Type.stratusTimePasswordToText
      $ Type.stratusTimeCredentialsPassword credentials
    ]
