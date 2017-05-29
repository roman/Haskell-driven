{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Driven.Internal.Worker where

import Protolude

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.QSemN (newQSemN, waitQSemN, signalQSemN)
import Data.HashMap.Strict (HashMap)

import qualified Data.Text.Encoding as Text (decodeUtf8)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as JSON

import Control.Driven.Internal.Types

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

workerHandler
  :: HashMap EventName [SomeEventHandler]
  -> WorkerMsg
  -> IO ()
workerHandler eventHandlerMap (WorkerMsg env payload deleteMsg) =
  let
    evName =
      fst $ weEventSpec env

    evSchema =
      esSchema $ snd $ weEventSpec env

    inputName =
      weInputName env

    outputMap =
      weOutputs env

    handleEvent
      :: SomeEventHandler
      -> IO [SomeOutputEvent]
    handleEvent someHandler =
      case (evSchema, someHandler) of
        (JsonSchema {}, JsonEventHandler handler) -> do
          weEmitEvent env (EventReceived inputName evName "json")
          case JSON.decodeStrict payload of
            Nothing -> do
              weEmitEvent
                env
                (InvalidEntryIgnored inputName evName "json" $ Text.decodeUtf8 payload)
              return []
            Just decodedPayload -> do
              eResult <- try $ handler (Msg deleteMsg decodedPayload)
              case eResult of
                Right result ->
                  return result
                Left (err :: SomeException) -> do
                  weEmitEvent env $ EventHandlerFailed inputName evName (show err)
                  return []

        (Protobuffer, ProtoEventHandler handler) -> do
          weEmitEvent env (EventReceived inputName evName "protobuffer")
          case fromProtobuff payload of
            Nothing -> do
              weEmitEvent
                env
                (InvalidEntryIgnored inputName evName "protobuffer" "<redacted>")
              return []

            Just decodedPayload -> do
              eResult <- try $ handler (Msg deleteMsg decodedPayload)
              case eResult of
                Right result ->
                  return result
                Left (err :: SomeException) -> do
                  weEmitEvent env $ EventHandlerFailed inputName evName (show err)
                  return []

        _ -> do
          weEmitEvent env $ EventFormatMissconfigured (weInputName env) evName
          return []

  in do
    case HashMap.lookup evName eventHandlerMap of
      Nothing ->
        weEmitEvent env $ EventHandlerMissconfigured (weInputName env) evName
      Just handlers -> do
        forM_ handlers $ \handler -> do
          outputEvents <- handleEvent handler
          mapM_ (_emitEvent outputMap) outputEvents

createWorker
  :: (DrivenEvent -> IO ())
  -> EventName
  -> EventSpec
  -> HashMap InputName Input
  -> HashMap EventName (EventSpec, [Output])
  -> (WorkerMsg -> IO ())
  -> WorkerSpec
  -> IO Worker
createWorker emitEvent evName evSpec allInputs outputsPerEvent msgHandler workerSpec = do
  evInputSource <- fetchWorkerInputSource workerSpec allInputs

  let
    inputName =
      wsInputName workerSpec

    workerEnv =
      WorkerEnv {
        weEventSpec = (evName, evSpec)
      , weOutputs   = outputsPerEvent
      , weInputName = inputName
      , weEmitEvent = emitEvent
      }

    callHandler = do
      (message, deleteMessage) <- readFromInput evInputSource
      async $ do
        let
          workerMsg =
            WorkerMsg {
              wmWorkerEnv = workerEnv
            , wmPayload = message
            , wmDeleteMsg = deleteMessage
            }
        msgHandler workerMsg

    cleanupWorker worker = do
      emitEvent (EventWorkerDisposed inputName evName)
      cancel worker

  workerSemaphore <- newQSemN (wsCount workerSpec)
  workerLoop <-
    async $ do
      emitEvent (EventWorkerCreated evName inputName)
      forever $ do
        waitQSemN workerSemaphore 1
        callHandler `finally` signalQSemN workerSemaphore 1

  return (Worker $ cleanupWorker workerLoop)
