module Revolio.Type.Vault
  ( Vault
  , makeVault
  , insertVault
  , lookupVault
  )
where

import qualified Control.Concurrent.STM as Stm
import qualified Data.Map as Map
import qualified Revolio.Type.SlackUserId as Type
import qualified Revolio.Type.StratusTimeLoginId as Type
import qualified Revolio.Type.StratusTimePassword as Type

type Vault
  = Stm.TVar
    ( Map.Map
        Type.SlackUserId
        (Type.StratusTimeLoginId, Type.StratusTimePassword)
    )

makeVault :: IO Vault
makeVault = Stm.newTVarIO Map.empty

insertVault
  :: Vault
  -> Type.SlackUserId
  -> Type.StratusTimeLoginId
  -> Type.StratusTimePassword
  -> IO ()
insertVault vault key username password =
  Stm.atomically . Stm.modifyTVar vault $ Map.insert key (username, password)

lookupVault
  :: Vault
  -> Type.SlackUserId
  -> IO
       ( Either
           String
           (Type.StratusTimeLoginId, Type.StratusTimePassword)
       )
lookupVault vault key = do
  map_ <- Stm.readTVarIO vault
  pure $ case Map.lookup key map_ of
    Nothing -> Left $ "missing required key: " <> show key
    Just value -> Right value
