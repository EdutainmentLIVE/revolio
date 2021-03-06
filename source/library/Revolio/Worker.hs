module Revolio.Worker
  ( runWorker
  )
where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.QueryLike as Http
import qualified Revolio.Type.Action as Type
import qualified Revolio.Type.Command as Type
import qualified Revolio.Type.Config as Type
import qualified Revolio.Type.Direction as Type
import qualified Revolio.Type.Payload as Type
import qualified Revolio.Type.Queue as Type
import qualified Revolio.Type.Slack.Message as Type
import qualified Revolio.Type.StratusTime.BaseUrl as Type
import qualified Revolio.Type.StratusTime.Credentials as Type
import qualified Revolio.Type.StratusTime.LoginId as Type
import qualified Revolio.Type.StratusTime.Password as Type
import qualified Revolio.Type.Url as Type
import qualified Revolio.Type.Vault as Type
import qualified Text.Printf as Printf

runWorker :: Type.Config -> Type.Queue -> Type.Vault -> IO ()
runWorker config queue vault = Monad.forever $ do
  payload <- Type.dequeue queue
  Exception.handle
    (handleException payload)
    (handlePayload config vault payload)

handleException :: Type.Payload -> Exception.SomeException -> IO ()
handleException payload (Exception.SomeException exception) =
  reply payload
    $ "Something went wrong: "
    <> Exception.displayException exception

handlePayload :: Type.Config -> Type.Vault -> Type.Payload -> IO ()
handlePayload config vault payload = case Type.payloadAction payload of
  Type.ActionHelp -> reply payload usageInfo

  Type.ActionSetup credentials -> do
    Type.insertVault vault (Type.payloadUserId payload) credentials
    reply payload "Successfully saved your credentials."

  Type.ActionClock direction -> do
    result <- Type.lookupVault vault $ Type.payloadUserId payload
    case result of
      Left _ -> reply payload "Failed to find your credentials."
      Right credentials -> do
        logInResponse <- logIn config credentials
        case getCookie logInResponse of
          Left _ -> reply payload "Failed to log in."
          Right cookie -> case getToken cookie of
            Left _ -> reply payload "Something went wrong after logging in."
            Right token -> do
              clockResponse <- clock config cookie token direction
              let
                io = case direction of
                  Type.DirectionIn -> "in"
                  Type.DirectionOut -> "out"
              if wasSaved clockResponse
                then reply payload $ "Successfully clocked " <> io <> "!"
                else reply payload $ "Failed to clock " <> io <> "."

reply :: Type.Payload -> String -> IO ()
reply payload message = do
  manager <- Tls.getGlobalManager
  request <-
    Client.parseRequest
    . Text.unpack
    . Type.urlToText
    $ Type.payloadResponseUrl payload
  Monad.void $ Client.httpLbs
    request
      { Client.method = Http.methodPost
      , Client.requestHeaders = [(Http.hContentType, jsonMime)]
      , Client.requestBody =
        Client.RequestBodyLBS
        . Aeson.encode
        . Type.textToSlackMessage
        $ Text.pack message
      }
    manager

usageInfo :: String
usageInfo =
  let
    format
      = "Usage:\n\
      \- `%s %s`: Show this help message\n\
      \- `%s %s`: Set up your username and password\n\
      \- `%s %s`: Clock in\n\
      \- `%s %s`: Clock out\n"
    c = Type.commandToText Type.CommandRevolio
    h = Type.actionToText Type.ActionHelp
    u = Type.textToStratusTimeLoginId $ Text.pack "USERNAME"
    p = Type.textToStratusTimePassword $ Text.pack "PASSWORD"
    s = Type.actionToText . Type.ActionSetup $ Type.StratusTimeCredentials u p
    i = Type.actionToText $ Type.ActionClock Type.DirectionIn
    o = Type.actionToText $ Type.ActionClock Type.DirectionOut
  in Printf.printf format c h c s c i c o

logIn
  :: Type.Config
  -> Type.StratusTimeCredentials
  -> IO (Client.Response LazyByteString.ByteString)
logIn config credentials = do
  manager <- Tls.getGlobalManager
  request <- makeRequest (Type.configUrl config) ""
  Client.httpLbs
    request
      { Client.method = Http.methodPost
      , Client.requestHeaders = [(Http.hContentType, formMime)]
      , Client.requestBody = Client.RequestBodyBS $ Http.renderQuery
        False
        [ (Http.toQueryKey "__VIEWSTATE", Http.toQueryValue "")
        , (Http.toQueryKey "btnLogin", Http.toQueryValue "login")
        , ( Http.toQueryKey "txtCustomerAlias"
          , Http.toQueryValue $ Type.configClient config
          )
        , ( Http.toQueryKey "txtLoginID"
          , Http.toQueryValue $ Type.stratusTimeCredentialsLoginId credentials
          )
        , ( Http.toQueryKey "txtPassword"
          , Http.toQueryValue $ Type.stratusTimeCredentialsPassword credentials
          )
        ]
      }
    manager

makeRequest :: Type.StratusTimeBaseUrl -> String -> IO Client.Request
makeRequest url path =
  Client.parseRequest $ Text.unpack (Type.stratusTimeBaseUrlToText url) <> path

getCookie :: Client.Response body -> Either String Client.Cookie
getCookie =
  maybe (Left "failed to log in") Right
    . Maybe.listToMaybe
    . filter (startsWithT . Client.cookie_name)
    . Client.destroyCookieJar
    . Client.responseCookieJar

startsWithT :: ByteString.ByteString -> Bool
startsWithT = ByteString.isPrefixOf $ ByteString.singleton 0x74

getToken :: Client.Cookie -> Either String ByteString.ByteString
getToken cookie = case ByteString.uncons $ Client.cookie_name cookie of
  Just (0x74, token) -> Right token
  _ -> Left "missing token"

clock
  :: Type.Config
  -> Client.Cookie
  -> ByteString.ByteString
  -> Type.Direction
  -> IO (Client.Response LazyByteString.ByteString)
clock config cookie token direction = do
  manager <- Tls.getGlobalManager
  request <- makeRequest
    (Type.configUrl config)
    "/EmployeeHome/EmployeeHome/AddPunch"
  Client.httpLbs
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

wasSaved :: Client.Response LazyByteString.ByteString -> Bool
wasSaved = (== punchWasSaved) . Client.responseBody

punchWasSaved :: LazyByteString.ByteString
punchWasSaved = LazyByteString.fromStrict $ utf8 "\xb2\"Punch was saved\"\xb2"

formMime :: ByteString.ByteString
formMime = utf8 "application/x-www-form-urlencoded"

jsonMime :: ByteString.ByteString
jsonMime = utf8 "application/json"

utf8 :: String -> ByteString.ByteString
utf8 = Encoding.encodeUtf8 . Text.pack
