module Revolio.Type.Config
  ( Config(..)
  , defaultConfig
  , getConfig
  )
where

import qualified Data.String as String
import qualified Data.Text as Text
import qualified Network.Wai.Handler.Warp as Warp
import qualified Revolio.Type.StratusTimeClientId as Type
import qualified Revolio.Type.SlackSigningSecret as Type
import qualified Revolio.Version as Version
import qualified System.Console.GetOpt as Console
import qualified Text.Read as Read

data Config = Config
  { configClient :: Type.StratusTimeClientId
  , configHost :: Warp.HostPreference
  , configPort :: Warp.Port
  , configSecret :: Type.SlackSigningSecret
  , configShowHelp :: Bool
  , configShowVersion :: Bool
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
  }

getConfig :: String -> [String] -> (String, Either String Config)
getConfig program arguments =
  let (us, as, os, es) = Console.getOpt' Console.Permute options arguments
  in (formatWarnings as os, buildConfig program us es)

type Option = Console.OptDescr Update

type Update = Config -> Either String Config

options :: [Option]
options =
  [ clientOption
  , helpOption
  , hostOption
  , portOption
  , secretOption
  , versionOption
  ]

clientOption :: Option
clientOption =
  makeOption ['c'] ["client"] "set the Stratus Time client ID"
    . requireArgument "CLIENT"
    $ \string config -> Right config
        { configClient = Type.textToStratusTimeClientId $ Text.pack string
        }

helpOption :: Option
helpOption =
  makeOption ['?'] ["help"] "show the help" . Console.NoArg $ \config ->
    Right config { configShowHelp = True }

hostOption :: Option
hostOption =
  makeOption ['h'] ["host"] "set the host"
    . requireArgument "HOST"
    $ \string config -> Right config { configHost = String.fromString string }

portOption :: Option
portOption =
  makeOption ['p'] ["port"] "set the port"
    . requireArgument "PORT"
    $ \string config -> case Read.readEither string of
        Left problem -> Left $ "invalid port: " <> problem
        Right port -> Right config { configPort = port }

secretOption :: Option
secretOption =
  makeOption ['s'] ["secret"] "set the Slack signing secret"
    . requireArgument "SECRET"
    $ \string config -> Right config
        { configSecret = Type.textToSlackSigningSecret $ Text.pack string
        }

versionOption :: Option
versionOption =
  makeOption ['v'] ["version"] "show the version" . Console.NoArg $ \config ->
    Right config { configShowVersion = True }

makeOption
  :: String -> [String] -> String -> Console.ArgDescr a -> Console.OptDescr a
makeOption short long description argument =
  Console.Option short long argument description

requireArgument :: String -> (String -> a) -> Console.ArgDescr a
requireArgument = flip Console.ReqArg

formatWarnings :: [String] -> [String] -> String
formatWarnings unexpectedArguments unknownOptions =
  unlines
    $ fmap formatUnexpectedArgument unexpectedArguments
    <> fmap formatUnknownOption unknownOptions

formatUnexpectedArgument :: String -> String
formatUnexpectedArgument = mappend "WARNING: unexpected argument " . quote

formatUnknownOption :: String -> String
formatUnknownOption = mappend "WARNING: unknown option " . quote

quote :: String -> String
quote string = "`" <> string <> "'"

buildConfig :: String -> [Update] -> [String] -> Either String Config
buildConfig program updates errors = if null errors
  then case foldr (either Left) (Right defaultConfig) updates of
    Left problem -> Left problem
    Right config
      | configShowHelp config -> Left $ showHelp program
      | configShowVersion config -> Left showVersion
      | otherwise -> Right config
  else Left $ concat errors

showHelp :: String -> String
showHelp program = Console.usageInfo program options

showVersion :: String
showVersion = Version.versionString <> "\n"
