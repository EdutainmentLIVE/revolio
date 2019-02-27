module Main
  ( main
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Crypto.Hash as Crypto
import qualified Crypto.MAC.HMAC as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray.Encoding as Memory
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Version as Version
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.URI as Uri
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified Paths_revolio as Package
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

main :: IO ()
main = do
  program <- Environment.getProgName
  arguments <- Environment.getArgs
  let (warnings, result) = getConfig program arguments
  IO.hPutStr IO.stderr warnings
  config <- case result of
    Left errors -> do
      IO.hPutStr IO.stderr errors
      Exit.exitFailure
    Right config -> pure config

  manager <- Tls.newTlsManager
  Tls.setGlobalManager manager

  queue <- Stm.newTBQueueIO 64
  vault <- Stm.newTVarIO Map.empty
  Async.race_ (server config queue) (worker config queue vault)

data Config = Config
  { configClient :: Text.Text
  , configHost :: Warp.HostPreference
  , configPort :: Warp.Port
  , configSecret :: ByteString.ByteString
  , configShowHelp :: Bool
  , configShowVersion :: Bool
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { configClient = Text.pack "0"
  , configHost = String.fromString "127.0.0.1"
  , configPort = 8080
  , configSecret = toUtf8 "00000000000000000000000000000000"
  , configShowHelp = False
  , configShowVersion = False
  }

getConfig :: String -> [String] -> (String, Either String Config)
getConfig program arguments =
  let
    (fs, as, os, es) = Console.getOpt' Console.Permute options arguments
    warnings =
      fmap (mappend "WARNING: unexpected argument " . quote) as
        <> fmap (mappend "WARNING: unknown option " . quote) os
    help = Console.usageInfo program options
    version = Version.showVersion Package.version <> "\n"
    result = if null es
      then case foldr (either Left) (Right defaultConfig) fs of
        Left errors -> Left errors
        Right config
          | configShowHelp config -> Left help
          | configShowVersion config -> Left version
          | otherwise -> Right config
      else Left $ concat es
  in (unlines warnings, result)

quote :: String -> String
quote x = "`" <> x <> "'"

type Option = Console.OptDescr (Config -> Either String Config)

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
clientOption = Console.Option
  ['c']
  ["client"]
  (Console.ReqArg
    (\string config -> pure config { configClient = Text.pack string })
    "CLIENT"
  )
  "set the Paychex client ID"

helpOption :: Option
helpOption = Console.Option
  ['?']
  ["help"]
  (Console.NoArg (\config -> pure config { configShowHelp = True }))
  "show the help"

hostOption :: Option
hostOption = Console.Option
  ['h']
  ["host"]
  (Console.ReqArg
    (\string config -> pure config { configHost = String.fromString string })
    "HOST"
  )
  "set the host"

portOption :: Option
portOption = Console.Option
  ['p']
  ["port"]
  (Console.ReqArg
    (\string config -> do
      port <- Read.readEither string
      pure config { configPort = port }
    )
    "PORT"
  )
  "set the port"

secretOption :: Option
secretOption = Console.Option
  ['s']
  ["secret"]
  (Console.ReqArg
    (\string config -> pure config { configSecret = toUtf8 string })
    "SECRET"
  )
  "set the Slack signing secret"

versionOption :: Option
versionOption = Console.Option
  ['v']
  ["version"]
  (Console.NoArg (\config -> pure config { configShowVersion = True }))
  "show the version"

type Queue = Stm.TBQueue Message

type Vault = Stm.TVar (Map.Map UserId (Text.Text, Text.Text))

server :: Config -> Queue -> IO ()
server config queue =
  Warp.runSettings (settings config) . middleware $ application config queue

settings :: Config -> Warp.Settings
settings config =
  Warp.setHost (configHost config)
    . Warp.setPort (configPort config)
    $ Warp.setServerName mempty Warp.defaultSettings

middleware :: Wai.Middleware
middleware = Middleware.logStdout

application :: Config -> Queue -> Wai.Application
application config queue request respond =
  let
    path = Text.unpack <$> Wai.pathInfo request
    method = Http.parseMethod $ Wai.requestMethod request
  in case path of
    [] -> case method of
      Right Http.POST -> handle config queue request respond
      _ -> respond $ Wai.responseBuilder Http.methodNotAllowed405 [] mempty
    _ -> respond $ Wai.responseBuilder Http.notFound404 [] mempty

handle :: Config -> Queue -> Wai.Application
handle config queue request respond = do
  body <- LazyByteString.toStrict <$> Wai.lazyRequestBody request
  case authorize config request body of
    Left problem ->
      respond
        . Wai.responseLBS Http.forbidden403 []
        . LazyByteString.fromStrict
        $ toUtf8 problem
    Right () -> case parsePayload $ Http.parseSimpleQuery body of
      Left problem ->
        respond
          . Wai.responseLBS Http.badRequest400 []
          . LazyByteString.fromStrict
          $ toUtf8 problem
      Right payload ->
        let command = Text.unpack $ payloadCommand payload
        in
          if command /= "/clock"
            then
              respond
              . Wai.responseLBS Http.badRequest400 []
              . LazyByteString.fromStrict
              . toUtf8
              $ "unknown command: "
              <> show command
            else
              let text = payloadText payload
              in
                case parseAction text of
                  Nothing ->
                    respond
                      . Wai.responseLBS Http.badRequest400 []
                      . LazyByteString.fromStrict
                      . toUtf8
                      $ "invalid text: "
                      <> show text
                  Just action -> do
                    Stm.atomically . Stm.writeTBQueue queue $ makeMessage
                      payload
                      action
                    respond . jsonResponse Http.ok200 [] $ stringToSlackMessage
                      "Working on it!"

jsonResponse
  :: Aeson.ToJSON body
  => Http.Status
  -> Http.ResponseHeaders
  -> body
  -> Wai.Response
jsonResponse status headers json = Wai.responseLBS
  status
  ((Http.hContentType, jsonMime) : headers)
  (Aeson.encode json)

authorize :: Config -> Wai.Request -> ByteString.ByteString -> Either String ()
authorize config request body = do
  let
    headers = Map.fromList $ Wai.requestHeaders request
    timestampKey = ci $ toUtf8 "X-Slack-Request-Timestamp"
    signatureKey = ci $ toUtf8 "X-Slack-Signature"
  timestamp <- case Map.lookup timestampKey headers of
    Nothing -> Left $ "missing key: " <> show timestampKey
    Just x -> Right x
  signature <- case Map.lookup signatureKey headers of
    Nothing -> Left $ "missing key: " <> show signatureKey
    Just byteString ->
      case ByteString.stripPrefix (toUtf8 "v0=") byteString of
        Nothing -> Left $ "malformed signature: " <> show byteString
        Just x -> Memory.convertFromBase Memory.Base16 x
  digest <- case Crypto.digestFromByteString signature of
    Nothing ->
      Left $ "invalid signature: " <> show (signature :: ByteString.ByteString)
    Just x -> Right x
  let
    expected = Crypto.HMAC digest :: Crypto.HMAC Crypto.SHA256
    message = toUtf8 "v0:" <> timestamp <> toUtf8 ":" <> body
    actual =
      Crypto.hmac (configSecret config) message :: Crypto.HMAC Crypto.SHA256
  if actual == expected then Right () else Left "not authorized"

toUtf8 :: String -> ByteString.ByteString
toUtf8 = Encoding.encodeUtf8 . Text.pack

ci :: CaseInsensitive.FoldCase string => string -> CaseInsensitive.CI string
ci = CaseInsensitive.mk

data Message = Message
  { messageAction :: Action
  , messageResponseUrl :: Uri.URI
  , messageUserId :: UserId
  } deriving (Eq, Show)

makeMessage :: Payload -> Action -> Message
makeMessage payload action = Message
  { messageAction = action
  , messageResponseUrl = payloadResponseUrl payload
  , messageUserId = payloadUserId payload
  }

data Action
  = Help
  | Setup Text.Text Text.Text
  | Punch Direction
  deriving (Eq, Show)

parseAction :: Text.Text -> Maybe Action
parseAction text = case Text.unpack <$> Text.words text of
  ["help"] -> Just Help
  ["in"] -> Just $ Punch In
  ["out"] -> Just $ Punch Out
  ["setup", username, password] ->
    Just $ Setup (Text.pack username) (Text.pack password)
  _ -> Nothing

data Direction = In | Out deriving (Eq, Show)

formMime :: ByteString.ByteString
formMime = toUtf8 "application/x-www-form-urlencoded"

data Payload = Payload
  { payloadCommand :: Text.Text
  , payloadResponseUrl :: Uri.URI
  , payloadText :: Text.Text
  , payloadUserId :: UserId
  } deriving (Eq, Show)

parsePayload :: Http.SimpleQuery -> Either String Payload
parsePayload query =
  let q = Map.fromList query
  in
    Payload
    <$> require q "command" parseText
    <*> require q "response_url" parseUri
    <*> require q "text" parseText
    <*> require q "user_id" (fmap UserId . parseText)

parseUri :: ByteString.ByteString -> Either String Uri.URI
parseUri byteString = do
  text <- parseText byteString
  case Uri.parseAbsoluteURI $ Text.unpack text of
    Nothing -> Left $ "invalid URI: " <> show text
    Just uri -> Right uri

parseText :: ByteString.ByteString -> Either String Text.Text
parseText byteString = case Encoding.decodeUtf8' byteString of
  Left unicodeException -> Left $ show unicodeException
  Right text -> Right text

require
  :: Map.Map ByteString.ByteString ByteString.ByteString
  -> String
  -> (ByteString.ByteString -> Either String a)
  -> Either String a
require query name convert =
  let key = toUtf8 name
  in
    case Map.lookup key query of
      Nothing -> Left $ "missing name: " <> show key
      Just value -> convert value

newtype UserId = UserId
  { userIdToText :: Text.Text
  } deriving (Eq, Ord, Show)

worker :: Config -> Queue -> Vault -> IO ()
worker config queue vault = Monad.forever $ do
  message <- Stm.atomically $ Stm.readTBQueue queue
  case messageAction message of
    Help -> reply
      message
      [ "Usage:"
      , "- `/clock help`: Show this help message"
      , "- `/clock setup USER PASS`: Set up your username and password"
      , "- `/clock in`: Punch in"
      , "- `/clock out`: Punch out"
      ]

    Setup username password -> do
      Stm.atomically . Stm.modifyTVar vault $ Map.insert
        (messageUserId message)
        (username, password)
      reply message ["Successfully saved your credentials."]

    Punch direction -> do
      Just (username, password) <- Map.lookup (messageUserId message)
        <$> Stm.readTVarIO vault
      (cookie, token) <- logIn config username password
      punch cookie token direction
      reply message ["Saved punch!"]

renderUri :: Uri.URI -> String
renderUri uri = Uri.uriToString id uri ""

newtype SlackMessage = SlackMessage
  { slackMessageToText :: Text.Text
  } deriving (Eq, Show)

instance Aeson.ToJSON SlackMessage where
  toJSON slackMessage = Aeson.object
    [ Text.pack "response_type" Aeson..= "ephemeral"
    , Text.pack "text" Aeson..= slackMessageToText slackMessage
    ]

stringToSlackMessage :: String -> SlackMessage
stringToSlackMessage = SlackMessage . Text.pack

jsonMime :: ByteString.ByteString
jsonMime = toUtf8 "application/json"

reply :: Message -> [String] -> IO ()
reply message strings = do
  manager <- Tls.getGlobalManager
  request <- Client.parseUrlThrow . renderUri $ messageResponseUrl message
  Monad.void $ Client.httpLbs
    request
      { Client.method = Http.methodPost
      , Client.requestHeaders = [(Http.hContentType, jsonMime)]
      , Client.requestBody =
        Client.RequestBodyLBS . Aeson.encode . stringToSlackMessage $ unlines
          strings
      }
    manager

logIn
  :: Config
  -> Text.Text
  -> Text.Text
  -> IO (Client.Cookie, ByteString.ByteString)
logIn config username password = do
  manager <- Tls.getGlobalManager
  request <- Client.parseUrlThrow "https://paychex.cloud.centralservers.com"
  response <- Client.httpLbs
    request
      { Client.method = Http.methodPost
      , Client.requestHeaders = [(Http.hContentType, formMime)]
      , Client.requestBody =
        Client.RequestBodyBS . Http.renderQuery False $ Http.toQuery
          [ ("__VIEWSTATE", "")
          , ("btnLogin", "Login")
          , ("txtCustomerAlias", Text.unpack $ configClient config)
          , ("txtLoginID", Text.unpack username)
          , ("txtPassword", Text.unpack password)
          ]
      }
    manager
  cookie <-
    maybe (fail "missing cookie") pure
    . Maybe.listToMaybe
    . filter
        (ByteString.isPrefixOf (ByteString.singleton 0x74) . Client.cookie_name
        )
    . Client.destroyCookieJar
    $ Client.responseCookieJar response
  token <- case ByteString.uncons $ Client.cookie_name cookie of
    Just (0x74, t) -> pure t
    _ -> fail "missing token"
  pure (cookie, token)

punch :: Client.Cookie -> ByteString.ByteString -> Direction -> IO ()
punch cookie token direction = do
  manager <- Tls.getGlobalManager
  request <-
    Client.parseUrlThrow
      "https://paychex.cloud.centralservers.com/EmployeeHome/EmployeeHome/AddPunch"
  response <- Client.httpLbs
    request
      { Client.method = Http.methodPost
      , Client.queryString = Http.renderQuery False
        $ Http.toQuery [("t", token)]
      , Client.requestHeaders = [(Http.hContentType, formMime)]
      , Client.cookieJar = Just $ Client.createCookieJar [cookie]
      , Client.requestBody =
        Client.RequestBodyBS . Http.renderQuery False $ Http.toQuery
          [ ( "TransactionType"
            , case direction of
              In -> "2"
              Out -> "3"
            )
          ]
      }
    manager
  Monad.when
      (Client.responseBody response
      /= LazyByteString.fromStrict (toUtf8 "\xb2\"Punch was saved\"\xb2")
      )
    $ fail "something went wrong"
