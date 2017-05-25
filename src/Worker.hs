{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Worker where

import Protolude

import Unsafe.Coerce (unsafeCoerce)

import GHC.TypeLits (KnownSymbol, SomeSymbol(..), someSymbolVal, sameSymbol)

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.QSemN (newQSemN, waitQSemN, signalQSemN)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Config

fetchWorkerInputSource
  :: EventName
  -> WorkerSpec
  -> HashMap InputName Input
  -> IO Input
fetchWorkerInputSource evName workerSpec allInputs =
  let
    inputName =
      wsInputName workerSpec

  in
    maybe (throwIO $ InputNameNotFound evName inputName)
          return
          (HashMap.lookup inputName allInputs)

createWorker
  :: (EventName, EventSpec)
  -> WorkerSpec
  -> HashMap InputName Input
  -> HashMap EventName (EventSpec, [Output])
  -> (WorkerMsg -> IO ())
  -> IO Worker
createWorker (evName, evSpec) workerSpec allInputs outputsPerEvent msgHandler = do
  evInputSource <- fetchWorkerInputSource evName workerSpec allInputs

  let
    workerEnv =
      WorkerEnv {
        weEventSpec = (evName, evSpec)
      , weOutputs   = outputsPerEvent
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

  workerSemaphore <- newQSemN (wsCount workerSpec)
  workerLoop <-
    async $ forever $ do
      waitQSemN workerSemaphore 1
      callHandler `finally` signalQSemN workerSemaphore 1

  return (Worker (cancel workerLoop))

workerHandler :: HashMap EventName [SomeEventHandler] -> WorkerMsg -> IO ()
workerHandler eventHandlerMap (WorkerMsg env payload deleteMsg) =
  let
    evName =
      fst $ weEventSpec env

    evSchema =
      case esSchema $ snd $ weEventSpec env of
        JsonSchema _ ->
          SJson

    outputMap =
      weOutputs env

    handleEvent
      :: ( KnownSymbol evId
         , IEventHandler evId
         , InputEventConstraint schema (Event evId)
         )
      => SSchema schema -> Proxy evId -> SomeEventHandler -> IO [SomeOutputEvent]
    handleEvent _schema inputProxy (SomeEventHandler innerSchema handlerProxy) =
      if isJust (sameSymbol handlerProxy inputProxy) then
        case _parseEvent innerSchema handlerProxy payload of
          Nothing -> do
            putStrLn $ "WARNING: Expecting Event " <> evName <> " to have a handler, but didn't"
            return []
          Just event ->
            _handleEvent handlerProxy event
      else
        return []

  in
    case someSymbolVal (Text.unpack evName) of
      SomeSymbol inputProxy ->
        case HashMap.lookup evName eventHandlerMap of
          Nothing ->
            -- TODO: Log warning
            return ()
          Just handlers ->
            forM_ handlers $ \handler -> do
              outputEvents <- handleEvent evSchema inputProxy handler
              mapM_ (_emitEvent evSchema outputMap) outputEvents
