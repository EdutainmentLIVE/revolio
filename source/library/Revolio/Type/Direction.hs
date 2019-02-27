module Revolio.Type.Direction
  ( Direction(..)
  , textToDirection
  , directionToText
  )
where

import qualified Data.Text as Text
import qualified Network.HTTP.Types.QueryLike as Http

data Direction
  = DirectionIn
  | DirectionOut
  deriving (Eq, Show)

instance Http.QueryValueLike Direction where
  toQueryValue direction = Http.toQueryValue $ case direction of
    DirectionIn -> "2"
    DirectionOut -> "3"

textToDirection :: Text.Text -> Either String Direction
textToDirection text = case Text.unpack text of
  "in" -> Right DirectionIn
  "out" -> Right DirectionOut
  _ -> Left $ "unknown direction: " <> show text

directionToText :: Direction -> Text.Text
directionToText direction = Text.pack $ case direction of
  DirectionIn -> "in"
  DirectionOut -> "out"
