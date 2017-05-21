{-# LANGUAGE NoImplicitPrelude #-}
module Worker where

import Protolude
import Control.Concurrent.Async (async, cancel)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Concurrent.QSemN (newQSemN, waitQSemN, signalQSemN)

import Config

data Worker
  = Worker {
    cancelWorker  :: IO ()
  , writeToQueue  :: ByteString -> IO ()
  }

toWorkerLoop
  :: WorkerSpec
  -> (WorkerSpec -> ByteString -> IO ())
  -> IO Worker
toWorkerLoop workerSpec msgHandler = do
  -- TODO: add memory queue size to config parameters
  workerQueue     <- newTBQueueIO 1000
  workerSemaphore <- newQSemN (iqWorkerCount (wsInputQueue workerSpec))
  workerLoop <-
    async
      $ forever
      $ bracket_ (waitQSemN workerSemaphore 1) (signalQSemN workerSemaphore 1)
      $ do input <- atomically $ readTBQueue workerQueue
           void $ async $ msgHandler workerSpec input

  return (Worker (cancel workerLoop)
                 (atomically . writeTBQueue workerQueue))
