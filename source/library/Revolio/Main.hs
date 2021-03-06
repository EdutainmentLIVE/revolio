module Revolio.Main
  ( defaultMain
  )
where

import qualified Control.Concurrent.Async as Async
import qualified Network.HTTP.Client.TLS as Tls
import qualified Revolio.Console as Console
import qualified Revolio.Server as Server
import qualified Revolio.Type.Queue as Type
import qualified Revolio.Type.Vault as Type
import qualified Revolio.Worker as Worker
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  program <- Environment.getProgName
  arguments <- Environment.getArgs

  let (warnings, result) = Console.getConfig program arguments
  IO.hPutStr IO.stderr warnings
  config <- either Exit.die pure result

  manager <- Tls.newTlsManager
  Tls.setGlobalManager manager

  queue <- Type.makeQueue
  vault <- Type.makeVault
  Async.race_
    (Server.runServer config queue)
    (Worker.runWorker config queue vault)
