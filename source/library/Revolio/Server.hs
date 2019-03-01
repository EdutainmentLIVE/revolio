module Revolio.Server
  ( runServer
  , authorizeRequest
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
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified Revolio.Type.Config as Type
import qualified Revolio.Type.Payload as Type
import qualified Revolio.Type.Queue as Type
import qualified Revolio.Type.Slack.Message as Type
import qualified Revolio.Type.Slack.SigningSecret as Type
import qualified Text.Printf as Printf

runServer :: Type.Config -> Type.Queue -> IO ()
runServer config queue =
  Warp.runSettings (settings config) . middleware $ application
    (Type.configSecret config)
    queue

settings :: Type.Config -> Warp.Settings
settings config =
  Warp.setBeforeMainLoop (beforeMainLoop config)
    . Warp.setHost (Type.configHost config)
    . Warp.setPort (Type.configPort config)
    . Warp.setOnExceptionResponse onExceptionResponse
    $ Warp.setServerName ByteString.empty Warp.defaultSettings

beforeMainLoop :: Type.Config -> IO ()
beforeMainLoop config = do
  let
    host = formatHost $ Type.configHost config
    port = Type.configPort config
  Printf.printf "Listening on %s:%d\n" host port

formatHost :: Warp.HostPreference -> String
formatHost host
  | host == String.fromString "*" = "*"
  | host == String.fromString "*4" = "*4"
  | host == String.fromString "!4" = "!4"
  | host == String.fromString "*6" = "*6"
  | host == String.fromString "!6" = "!6"
  | otherwise = dropEnd 1 . drop 6 $ show host

dropEnd :: Int -> [a] -> [a]
dropEnd n l = fmap fst . zip l $ drop n l

onExceptionResponse :: exception -> Wai.Response
onExceptionResponse _ = jsonResponse Http.internalServerError500 [] Aeson.Null

middleware :: Wai.Middleware
middleware = Middleware.logStdout

application :: Type.SlackSigningSecret -> Type.Queue -> Wai.Application
application secret queue request respond = do
  let
    path = Text.unpack <$> Wai.pathInfo request
    method = Http.parseMethod $ Wai.requestMethod request
  response <- case path of
    [] -> case method of
      Right Http.GET -> pure $ jsonResponse
        Http.ok200
        []
        "How familiar are you with the gear wars, exactly?"
      _ -> pure $ jsonResponse Http.methodNotAllowed405 [] Aeson.Null
    ["ping"] -> case method of
      Right Http.GET -> pure $ jsonResponse Http.ok200 [] Aeson.Null
      _ -> pure $ jsonResponse Http.methodNotAllowed405 [] Aeson.Null
    ["slack"] -> case method of
      Right Http.POST -> handler secret queue request
      _ -> pure $ jsonResponse Http.methodNotAllowed405 [] Aeson.Null
    _ -> pure $ jsonResponse Http.notFound404 [] Aeson.Null
  respond response

handler
  :: Type.SlackSigningSecret -> Type.Queue -> Wai.Request -> IO Wai.Response
handler secret queue request = do
  body <- LazyByteString.toStrict <$> Wai.lazyRequestBody request
  case authorizeRequest secret request body of
    Left problem -> pure $ jsonResponse Http.forbidden403 [] problem
    Right () -> case Type.queryToPayload body of
      Left problem ->
        pure . jsonResponse Http.ok200 [] . Type.textToSlackMessage $ Text.pack
          problem
      Right payload -> do
        Type.enqueue queue payload
        pure . jsonResponse Http.ok200 [] . Type.textToSlackMessage $ Text.pack
          "Working on it!"

jsonResponse
  :: Aeson.ToJSON body
  => Http.Status
  -> Http.ResponseHeaders
  -> body
  -> Wai.Response
jsonResponse status headers json = Wai.responseLBS
  status
  ((Http.hContentType, utf8 "application/json") : headers)
  (Aeson.encode json)

authorizeRequest
  :: Type.SlackSigningSecret
  -> Wai.Request
  -> ByteString.ByteString
  -> Either String ()
authorizeRequest secret request body = do
  let headers = Map.fromList $ Wai.requestHeaders request
  timestamp <- getHeader headers "X-Slack-Request-Timestamp"
  let message = utf8 "v0:" <> timestamp <> utf8 ":" <> body
  digest <- getDigest headers
  if Crypto.hmac secret message == Crypto.HMAC digest
    then Right ()
    else Left "not authorized"

type Headers = Map.Map Http.HeaderName ByteString.ByteString

getDigest :: Headers -> Either String (Crypto.Digest Crypto.SHA256)
getDigest headers = do
  value <- getHeader headers "X-Slack-Signature"
  signature <- parseSignature value
  convertDigest signature

getHeader :: Headers -> String -> Either String ByteString.ByteString
getHeader headers name = case Map.lookup (ci $ utf8 name) headers of
  Nothing -> Left $ "missing required key: " <> show name
  Just value -> Right value

parseSignature :: ByteString.ByteString -> Either String ByteString.ByteString
parseSignature value = case ByteString.stripPrefix (utf8 "v0=") value of
  Nothing -> Left $ "malformed signature: " <> show value
  Just signature -> Memory.convertFromBase Memory.Base16 signature

convertDigest
  :: ByteString.ByteString -> Either String (Crypto.Digest Crypto.SHA256)
convertDigest signature = case Crypto.digestFromByteString signature of
  Nothing -> Left $ "invalid signature: " <> show signature
  Just digest -> Right digest

utf8 :: String -> ByteString.ByteString
utf8 = Encoding.encodeUtf8 . Text.pack

ci :: CaseInsensitive.FoldCase string => string -> CaseInsensitive.CI string
ci = CaseInsensitive.mk
