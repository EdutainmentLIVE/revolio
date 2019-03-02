module Revolio.Type.Vault
  ( Vault
  , makeVault
  , insertVault
  , lookupVault
  )
where

import qualified Control.Concurrent.STM as Stm
import qualified Data.Map as Map
import qualified Revolio.Type.Slack.UserId as Type
import qualified Revolio.Type.StratusTime.Credentials as Type

type Vault
  = Stm.TVar
    ( Map.Map
        Type.SlackUserId
        Type.StratusTimeCredentials
    )

makeVault :: IO Vault
makeVault = Stm.newTVarIO Map.empty

insertVault
  :: Vault
  -> Type.SlackUserId
  -> Type.StratusTimeCredentials
  -> IO ()
insertVault vault key value =
  Stm.atomically . Stm.modifyTVar vault $ Map.insert key value

lookupVault
  :: Vault
  -> Type.SlackUserId
  -> IO
       ( Either
           String
           Type.StratusTimeCredentials
       )
lookupVault vault key = do
  map_ <- Stm.readTVarIO vault
  pure $ case Map.lookup key map_ of
    Nothing -> Left $ "missing required key: " <> show key
    Just value -> Right value
