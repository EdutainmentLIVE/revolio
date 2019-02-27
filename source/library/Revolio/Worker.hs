module Revolio.Worker
  ( runWorker
  )
where

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
import qualified Revolio.Type as Type

toUtf8 :: String -> ByteString.ByteString
toUtf8 = Encoding.encodeUtf8 . Text.pack

formMime :: ByteString.ByteString
formMime = toUtf8 "application/x-www-form-urlencoded"

runWorker :: Type.PaychexClientId -> Type.Queue -> Type.Vault -> IO ()
runWorker client queue vault = Monad.forever $ do
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
      (cookie, token) <- logIn client username password
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
  :: Type.PaychexClientId
  -> Type.PaychexLoginId
  -> Type.PaychexPassword
  -> IO (Client.Cookie, ByteString.ByteString)
logIn client username password = do
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
          , ("txtCustomerAlias", Type.paychexClientIdToText client)
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
