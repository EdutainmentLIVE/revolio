module Revolio.Main
  ( defaultMain
  )
where

import qualified Control.Concurrent.Async as Async
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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified Revolio.Type as Type
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  program <- Environment.getProgName
  arguments <- Environment.getArgs

  let (warnings, result) = Type.getConfig program arguments
  IO.hPutStr IO.stderr warnings
  config <- either Exit.die pure result

  manager <- Tls.newTlsManager
  Tls.setGlobalManager manager

  queue <- Type.makeQueue
  vault <- Type.makeVault
  Async.race_ (server config queue) (worker config queue vault)

server :: Type.Config -> Type.Queue -> IO ()
server config queue =
  Warp.runSettings (settings config) . middleware $ application config queue

settings :: Type.Config -> Warp.Settings
settings config =
  Warp.setHost (Type.configHost config)
    . Warp.setPort (Type.configPort config)
    $ Warp.setServerName mempty Warp.defaultSettings

middleware :: Wai.Middleware
middleware = Middleware.logStdout

application :: Type.Config -> Type.Queue -> Wai.Application
application config queue request respond =
  let
    path = Text.unpack <$> Wai.pathInfo request
    method = Http.parseMethod $ Wai.requestMethod request
  in case path of
    [] -> case method of
      Right Http.POST -> handle config queue request respond
      _ -> respond $ Wai.responseBuilder Http.methodNotAllowed405 [] mempty
    _ -> respond $ Wai.responseBuilder Http.notFound404 [] mempty

handle :: Type.Config -> Type.Queue -> Wai.Application
handle config queue request respond = do
  body <- LazyByteString.toStrict <$> Wai.lazyRequestBody request
  case authorize config request body of
    Left problem ->
      respond
        . Wai.responseLBS Http.forbidden403 []
        . LazyByteString.fromStrict
        $ toUtf8 problem
    Right () -> case Type.queryToPayload body of
      Left problem ->
        respond
          . Wai.responseLBS Http.badRequest400 []
          . LazyByteString.fromStrict
          $ toUtf8 problem
      Right payload -> do
        Type.enqueue queue payload
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

authorize
  :: Type.Config -> Wai.Request -> ByteString.ByteString -> Either String ()
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
      Crypto.hmac (Type.configSecret config) message :: Crypto.HMAC
          Crypto.SHA256
  if actual == expected then Right () else Left "not authorized"

toUtf8 :: String -> ByteString.ByteString
toUtf8 = Encoding.encodeUtf8 . Text.pack

ci :: CaseInsensitive.FoldCase string => string -> CaseInsensitive.CI string
ci = CaseInsensitive.mk

formMime :: ByteString.ByteString
formMime = toUtf8 "application/x-www-form-urlencoded"

worker :: Type.Config -> Type.Queue -> Type.Vault -> IO ()
worker config queue vault = Monad.forever $ do
  payload <- Type.dequeue queue
  case Type.payloadAction payload of
    Type.ActionHelp -> reply
      payload
      [ "Usage:"
      , "- `/clock help`: Show this help message"
      , "- `/clock setup USER PASS`: Set up your username and password"
      , "- `/clock in`: Punch in"
      , "- `/clock out`: Punch out"
      ]

    Type.ActionSetup username password -> do
      Type.insertVault vault (Type.payloadUserId payload) username password
      reply payload ["Successfully saved your credentials."]

    Type.ActionClock direction -> do
      Right (username, password) <- Type.lookupVault vault
        $ Type.payloadUserId payload
      (cookie, token) <- logIn config username password
      punch cookie token direction
      reply payload ["Saved punch!"]

renderUri :: Type.Url -> String
renderUri = Text.unpack . Type.urlToText

stringToSlackMessage :: String -> Type.SlackMessage
stringToSlackMessage = Type.textToSlackMessage . Text.pack

jsonMime :: ByteString.ByteString
jsonMime = toUtf8 "application/json"

reply :: Type.Payload -> [String] -> IO ()
reply payload strings = do
  manager <- Tls.getGlobalManager
  request <- Client.parseUrlThrow . renderUri $ Type.payloadResponseUrl payload
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
  :: Type.Config
  -> Type.PaychexLoginId
  -> Type.PaychexPassword
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
          [ ("__VIEWSTATE", Text.empty)
          , ("btnLogin", Text.pack "Login")
          , ( "txtCustomerAlias"
            , Type.paychexClientIdToText $ Type.configClient config
            )
          , ("txtLoginID", Type.paychexLoginIdToText username)
          , ("txtPassword", Type.paychexPasswordToText password)
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

punch :: Client.Cookie -> ByteString.ByteString -> Type.Direction -> IO ()
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
          [("TransactionType", direction)]
      }
    manager
  Monad.when
      (Client.responseBody response
      /= LazyByteString.fromStrict (toUtf8 "\xb2\"Punch was saved\"\xb2")
      )
    $ fail "something went wrong"
