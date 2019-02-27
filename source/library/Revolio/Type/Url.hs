module Revolio.Type.Url
  ( Url
  , textToUrl
  , urlToText
  )
where

import qualified Data.Text as Text
import qualified Network.URI as Uri

newtype Url
  = Url Uri.URI
  deriving (Eq, Show)

textToUrl :: Text.Text -> Either String Url
textToUrl text = case Uri.parseAbsoluteURI $ Text.unpack text of
  Nothing -> Left $ "invalid Url: " <> show text
  Just uri -> Right $ Url uri

urlToText :: Url -> Text.Text
urlToText (Url uri) = Text.pack $ Uri.uriToString id uri ""
