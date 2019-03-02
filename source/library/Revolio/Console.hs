module Revolio.Console
  ( getConfig
  )
where

import qualified Data.String as String
import qualified Data.Text as Text
import qualified Revolio.Type.Config as Type
import qualified Revolio.Type.Slack.SigningSecret as Type
import qualified Revolio.Type.StratusTime.BaseUrl as Type
import qualified Revolio.Type.StratusTime.ClientId as Type
import qualified Revolio.Version as Version
import qualified System.Console.GetOpt as Console
import qualified Text.Read as Read

getConfig :: String -> [String] -> (String, Either String Type.Config)
getConfig program arguments =
  let (us, as, os, es) = Console.getOpt' Console.Permute options arguments
  in (formatWarnings as os, buildConfig program us es)

type Option = Console.OptDescr Update

type Update = Type.Config -> Either String Type.Config

options :: [Option]
options =
  [ clientOption
  , helpOption
  , hostOption
  , portOption
  , secretOption
  , urlOption
  , versionOption
  ]

clientOption :: Option
clientOption =
  makeOption ['c'] ["client"] "set the Stratus Time client ID"
    . requireArgument "CLIENT"
    $ \string config -> Right config
        { Type.configClient = Type.textToStratusTimeClientId $ Text.pack string
        }

helpOption :: Option
helpOption =
  makeOption ['?'] ["help"] "show the help" . Console.NoArg $ \config ->
    Right config { Type.configShowHelp = True }

hostOption :: Option
hostOption =
  makeOption ['h'] ["host"] "set the host"
    . requireArgument "HOST"
    $ \string config ->
        Right config { Type.configHost = String.fromString string }

portOption :: Option
portOption =
  makeOption ['p'] ["port"] "set the port"
    . requireArgument "PORT"
    $ \string config -> case Read.readEither string of
        Left problem -> Left $ "invalid port: " <> problem
        Right port -> Right config { Type.configPort = port }

secretOption :: Option
secretOption =
  makeOption ['s'] ["secret"] "set the Slack signing secret"
    . requireArgument "SECRET"
    $ \string config -> Right config
        { Type.configSecret = Type.textToSlackSigningSecret $ Text.pack string
        }

urlOption :: Option
urlOption =
  makeOption ['u'] ["url"] "set the Stratus Time base URL"
    . requireArgument "URL"
    $ \string config -> Right config
        { Type.configUrl = Type.textToStratusTimeBaseUrl $ Text.pack string
        }

versionOption :: Option
versionOption =
  makeOption ['v'] ["version"] "show the version" . Console.NoArg $ \config ->
    Right config { Type.configShowVersion = True }

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

buildConfig :: String -> [Update] -> [String] -> Either String Type.Config
buildConfig program updates errors = if null errors
  then case foldr (either Left) (Right Type.defaultConfig) updates of
    Left problem -> Left problem
    Right config
      | Type.configShowHelp config -> Left $ showHelp program
      | Type.configShowVersion config -> Left showVersion
      | otherwise -> Right config
  else Left $ concat errors

showHelp :: String -> String
showHelp program = Console.usageInfo program options

showVersion :: String
showVersion = Version.versionString <> "\n"
