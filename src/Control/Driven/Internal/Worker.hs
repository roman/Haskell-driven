{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Driven.Internal.Worker where

import Protolude

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Data.HashMap.Strict      (HashMap)

import qualified Data.Aeson          as JSON
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Encoding  as Text (decodeUtf8)

import Control.Driven.Internal.Types

fetchWorkerInputSource
  :: WorkerSpec
  -> HashMap InputName Input
  -> IO Input
fetchWorkerInputSource workerSpec inputMap =
  let
    inputName =
      wsInputName workerSpec

  in
    maybe (throwIO $ InputNameNotFound inputName)
          return
          (HashMap.lookup inputName inputMap)

workerHandler
  :: HashMap EventName Schema
  -> HashMap EventName [SomeEventHandler]
  -> WorkerMsg
  -> IO ()
workerHandler schemaMap eventHandlerMap (WorkerMsg env inputBytes deleteMsg) =
  let
    evName =
      fst $ weEventSpec env

    inputName =
      weInputName env

    outputMap =
      weOutputs env

    emitOutputEvent outputEvent =
      let
        mSchema =
          HashMap.lookup evName schemaMap

        mOutputs =
          HashMap.lookup evName outputMap

      in case (mSchema, mOutputs) of
        (Nothing, _) ->
          weEmitDrivenEvent env (EventSchemaMissconfigured evName)

        (_, Nothing) ->
          weEmitDrivenEvent env (EventOutputMissconfigured evName)

        (Just checkSchema, Just (_, outputList)) ->
          let
            outputBytes =
              serializeEvent outputEvent

          in
            case checkSchema outputBytes of
              Left err ->
                weEmitDrivenEvent env (EventSchemaMissmatch evName (show err))

              Right _ ->
                mapM_ (`writeToOutput` outputBytes) outputList

    runEventHandler :: SomeEventHandler -> IO ()
    runEventHandler someHandler = do
      weEmitDrivenEvent env (EventReceived inputName evName (handlerTypeName someHandler))
      handlerResult <- try $ handleEvent someHandler inputBytes
      case handlerResult of
        Left (err :: SomeException) -> do
          weEmitDrivenEvent env (EventHandlerFailed inputName evName $ show err)

        Right outputEventList -> do
          mapM_ emitOutputEvent outputEventList
          deleteMsg

  in
    case HashMap.lookup evName eventHandlerMap of
      Nothing ->
        weEmitDrivenEvent env $ EventHandlerMissconfigured (weInputName env) evName

      Just handlers ->
        mapM_ runEventHandler handlers

createWorker
  :: (DrivenEvent -> IO ())
  -> EventName
  -> EventSpec
  -> WorkerSpec
  -> HashMap InputName Input
  -> HashMap EventName (EventSpec, [Output])
  -> (WorkerMsg -> IO ())
  -> IO Worker
createWorker emitDrivenEvent evName evSpec workerSpec inputMap outputsPerEvent msgHandler = do
  evInputSource <- fetchWorkerInputSource workerSpec inputMap

  let
    inputName =
      wsInputName workerSpec

    workerEnv =
      WorkerEnv {
        weEventSpec = (evName, evSpec)
      , weOutputs   = outputsPerEvent
      , weInputName = inputName
      , weEmitDrivenEvent = emitDrivenEvent
      }

    callHandler = do
      (message, deleteMsg) <- readFromInput evInputSource
      async $ do
        let
          workerMsg =
            WorkerMsg {
              wmWorkerEnv = workerEnv
            , wmPayload = message
            , wmDeleteMsg = deleteMsg
            }
        msgHandler workerMsg

    cleanupWorker worker = do
      emitDrivenEvent (EventWorkerDisposed inputName evName)
      cancel worker

  workerSemaphore <- newQSemN (wsCount workerSpec)
  workerLoop <-
    async $ do
      emitDrivenEvent (EventWorkerCreated evName inputName)
      forever $ do
        waitQSemN workerSemaphore 1
        callHandler `finally` signalQSemN workerSemaphore 1

  return (Worker $ cleanupWorker workerLoop)
