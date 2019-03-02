module Revolio.Type.Config
  ( Config(..)
  , defaultConfig
  )
where

import qualified Data.String as String
import qualified Data.Text as Text
import qualified Network.Wai.Handler.Warp as Warp
import qualified Revolio.Type.Slack.SigningSecret as Type
import qualified Revolio.Type.StratusTime.BaseUrl as Type
import qualified Revolio.Type.StratusTime.ClientId as Type

data Config = Config
  { configClient :: Type.StratusTimeClientId
  , configHost :: Warp.HostPreference
  , configPort :: Warp.Port
  , configSecret :: Type.SlackSigningSecret
  , configShowHelp :: Bool
  , configShowVersion :: Bool
  , configUrl :: Type.StratusTimeBaseUrl
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { configClient = Type.textToStratusTimeClientId
    $ Text.pack "arbitrary-stratus-time-client-id"
  , configHost = String.fromString "127.0.0.1"
  , configPort = 8080
  , configSecret = Type.textToSlackSigningSecret
    $ Text.pack "arbitrary-slack-signing-secret"
  , configShowHelp = False
  , configShowVersion = False
  , configUrl = Type.textToStratusTimeBaseUrl
    $ Text.pack "https://stratustime.centralservers.com"
  }
