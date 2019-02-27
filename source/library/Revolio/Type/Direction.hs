module Revolio.Type.Direction
  ( Direction(..)
  )
where

import qualified Network.HTTP.Types.QueryLike as Http

data Direction
  = DirectionIn
  | DirectionOut
  deriving (Eq, Show)

instance Http.QueryValueLike Direction where
  toQueryValue direction = Http.toQueryValue $ case direction of
    DirectionIn -> "2"
    DirectionOut -> "3"
