{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Worker where

import Protolude

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.QSemN (newQSemN, waitQSemN, signalQSemN)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HashMap

import Config
import Input

data Worker
  = Worker {
    cancelWorker  :: IO ()
  }

data WorkerEnv
  = WorkerEnv {
    weEventSpec :: EventSpec
  , weInputSpec :: InputSpec
  , weInput     :: Input
  }

data WorkerMsg
  = WorkerMsg {
    wmWorkerEnv :: WorkerEnv
  , wmPayload   :: ByteString
  , wmDeleteMsg :: IO ()
  }

aggregateWorkersPerInput :: EventSpec -> HashMap InputSpec [WorkerSpec]
aggregateWorkersPerInput eventSpec =
  let
    step inputSpec acc =
      let
        workerSpec =
          iqWorkerSpec inputSpec
      in
        HashMap.alter (return . maybe [workerSpec] (workerSpec:)) inputSpec acc
  in
    foldr step HashMap.empty (esInputSpec eventSpec)

createWorker
  :: EventSpec
  -> InputSpec
  -> Input
  -> (WorkerMsg -> IO ())
  -> IO Worker
createWorker eventSpec inputSpec inputSource msgHandler =
  let
    workerSpec =
      iqWorkerSpec inputSpec

    workerEnv =
      WorkerEnv {
        weEventSpec = eventSpec
      , weInputSpec = inputSpec
      , weInput     = inputSource
      }

    callHandler = do
      (message, deleteMessage) <- readFromInput inputSource
      async $ do
        let
          workerMsg =
            WorkerMsg {
              wmWorkerEnv = workerEnv
            , wmPayload = message
            , wmDeleteMsg = deleteMessage
            }
        msgHandler workerMsg

  in do
    workerSemaphore <- newQSemN (wsCount workerSpec)
    workerLoop <-
      async $ forever $ do
        waitQSemN workerSemaphore 1
        callHandler `finally` signalQSemN workerSemaphore 1

    return (Worker (cancel workerLoop))
