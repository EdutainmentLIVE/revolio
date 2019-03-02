module Revolio.Type.StratusTime.Credentials
  ( StratusTimeCredentials(..)
  )
where

import qualified Revolio.Type.StratusTime.LoginId as Type
import qualified Revolio.Type.StratusTime.Password as Type

data StratusTimeCredentials = StratusTimeCredentials
  { stratusTimeCredentialsLoginId :: Type.StratusTimeLoginId
  , stratusTimeCredentialsPassword :: Type.StratusTimePassword
  } deriving (Eq, Show)
