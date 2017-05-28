{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Worker where

import Protolude

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.QSemN (newQSemN, waitQSemN, signalQSemN)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as JSON

import Config

fetchWorkerInputSource
  :: WorkerSpec
  -> HashMap InputName Input
  -> IO Input
fetchWorkerInputSource workerSpec allInputs =
  let
    inputName =
      wsInputName workerSpec

  in
    maybe (throwIO $ InputNameNotFound inputName)
          return
          (HashMap.lookup inputName allInputs)

workerHandler :: HashMap EventName [SomeEventHandler] -> WorkerMsg -> IO ()
workerHandler eventHandlerMap (WorkerMsg env payload deleteMsg) =
  let
    evName =
      fst $ weEventSpec env

    evSchema =
      esSchema $ snd $ weEventSpec env

    outputMap =
      weOutputs env

    handleEvent
      :: SomeEventHandler -> IO [SomeOutputEvent]
    handleEvent someHandler =
      case (evSchema, someHandler) of
        (JsonSchema {}, JsonEventHandler handler) -> do
          putStrLn ("===> json" :: Text)
          maybe (putStrLn ("ignoring message: " <> payload) >> return [])
                (handler . Msg deleteMsg)
                (JSON.decodeStrict payload)
        (Protobuffer, ProtoEventHandler handler) -> do
          putStrLn ("===> proto" :: Text)
          maybe (putStrLn ("ignoring message: " <> payload) >> return [])
                (handler . Msg deleteMsg)
                (fromProtobuff payload)
        _ -> do
          putStrLn
            $ "WARNING: event " <> evName <> " has invalid format"
          return []

  in do
    putStrLn $ "===> " <> evName
    case HashMap.lookup evName eventHandlerMap of
      Nothing ->
        -- TODO: Log warning here with more context
        putStrLn ("Invalid event name received" :: Text)
      Just handlers -> do
        forM_ handlers $ \handler -> do
          outputEvents <- handleEvent handler
          mapM_ (_emitEvent outputMap) outputEvents

createWorker
  :: (EventName, EventSpec)
  -> WorkerSpec
  -> HashMap InputName Input
  -> HashMap EventName (EventSpec, [Output])
  -> (WorkerMsg -> IO ())
  -> IO Worker
createWorker (evName, evSpec) workerSpec allInputs outputsPerEvent msgHandler = do
  evInputSource <- fetchWorkerInputSource workerSpec allInputs

  let
    workerEnv =
      WorkerEnv {
        weEventSpec = (evName, evSpec)
      , weOutputs   = outputsPerEvent
      }

    callHandler = do
      (message, deleteMessage) <- readFromInput evInputSource
      print (evName, message)
      async $ do
        let
          workerMsg =
            WorkerMsg {
              wmWorkerEnv = workerEnv
            , wmPayload = message
            , wmDeleteMsg = deleteMessage
            }
        msgHandler workerMsg

  workerSemaphore <- newQSemN (wsCount workerSpec)
  workerLoop <-
    async $ forever $ do
      waitQSemN workerSemaphore 1
      callHandler `finally` signalQSemN workerSemaphore 1

  return (Worker (cancel workerLoop))
