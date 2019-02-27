module Revolio.Type.Payload
  ( Payload(..)
  , queryToPayload
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Types as Http
import qualified Revolio.Type.Action as Type
import qualified Revolio.Type.Command as Type
import qualified Revolio.Type.SlackUserId as Type
import qualified Revolio.Type.Url as Type

data Payload = Payload
  { payloadAction :: Type.Action
  , payloadCommand :: Type.Command
  , payloadResponseUrl :: Type.Url
  , payloadUserId :: Type.SlackUserId
  } deriving (Eq, Show)

queryToPayload :: ByteString.ByteString -> Either String Payload
queryToPayload query = do
  map_ <- parseQuery query
  parsePayload map_

parseQuery
  :: ByteString.ByteString -> Either String (Map.Map Text.Text Text.Text)
parseQuery =
  fmap Map.fromList . traverse parseQueryItem . Http.parseSimpleQuery

parseQueryItem :: Http.SimpleQueryItem -> Either String (Text.Text, Text.Text)
parseQueryItem (key, value) = (,) <$> parseText key <*> parseText value

parseText :: ByteString.ByteString -> Either String Text.Text
parseText byteString = case Encoding.decodeUtf8' byteString of
  Left problem ->
    Left $ "invalid UTF-8 (" <> show problem <> "): " <> show byteString
  Right text -> Right text

parsePayload :: Map.Map Text.Text Text.Text -> Either String Payload
parsePayload query = do
  action <- fetch query "text" Type.textToAction
  command <- fetch query "command" Type.textToCommand
  responseUrl <- fetch query "response_url" Type.textToUrl
  userId <- fetch query "user_id" $ pure . Type.textToSlackUserId
  Right Payload
    { payloadAction = action
    , payloadCommand = command
    , payloadResponseUrl = responseUrl
    , payloadUserId = userId
    }

fetch
  :: Map.Map Text.Text Text.Text
  -> String
  -> (Text.Text -> Either String a)
  -> Either String a
fetch query key convert = case Map.lookup (Text.pack key) query of
  Nothing -> Left $ "missing required key: " <> show key
  Just value -> convert value
