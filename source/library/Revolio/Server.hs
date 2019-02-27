module Revolio.Server
  ( runServer
  )
where

import qualified Crypto.Hash as Crypto
import qualified Crypto.MAC.HMAC as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray.Encoding as Memory
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified Revolio.Type as Type

runServer :: Type.Config -> Type.Queue -> IO ()
runServer config queue =
  Warp.runSettings (settings config) . middleware $ application
    (Type.configSecret config)
    queue

settings :: Type.Config -> Warp.Settings
settings config =
  Warp.setHost (Type.configHost config)
    . Warp.setPort (Type.configPort config)
    $ Warp.setServerName mempty Warp.defaultSettings

middleware :: Wai.Middleware
middleware = Middleware.logStdout

application :: Type.SlackSigningSecret -> Type.Queue -> Wai.Application
application secret queue request respond =
  let
    path = Text.unpack <$> Wai.pathInfo request
    method = Http.parseMethod $ Wai.requestMethod request
  in case path of
    [] -> case method of
      Right Http.POST -> handle secret queue request respond
      _ -> respond $ Wai.responseBuilder Http.methodNotAllowed405 [] mempty
    _ -> respond $ Wai.responseBuilder Http.notFound404 [] mempty

handle :: Type.SlackSigningSecret -> Type.Queue -> Wai.Application
handle secret queue request respond = do
  body <- LazyByteString.toStrict <$> Wai.lazyRequestBody request
  case authorize secret request body of
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
  :: Type.SlackSigningSecret
  -> Wai.Request
  -> ByteString.ByteString
  -> Either String ()
authorize secret request body = do
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
    actual = Crypto.hmac secret message :: Crypto.HMAC Crypto.SHA256
  if actual == expected then Right () else Left "not authorized"

toUtf8 :: String -> ByteString.ByteString
toUtf8 = Encoding.encodeUtf8 . Text.pack

ci :: CaseInsensitive.FoldCase string => string -> CaseInsensitive.CI string
ci = CaseInsensitive.mk

stringToSlackMessage :: String -> Type.SlackMessage
stringToSlackMessage = Type.textToSlackMessage . Text.pack

jsonMime :: ByteString.ByteString
jsonMime = toUtf8 "application/json"
