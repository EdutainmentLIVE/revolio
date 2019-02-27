module Revolio.Type.Queue
  ( Queue
  , makeQueue
  , enqueue
  , dequeue
  )
where

import qualified Control.Concurrent.STM as Stm
import qualified Revolio.Type.Payload as Type

type Queue = Stm.TBQueue Type.Payload

makeQueue :: IO Queue
makeQueue = Stm.newTBQueueIO 64

enqueue :: Queue -> Type.Payload -> IO ()
enqueue queue = Stm.atomically . Stm.writeTBQueue queue

dequeue :: Queue -> IO Type.Payload
dequeue = Stm.atomically . Stm.readTBQueue
