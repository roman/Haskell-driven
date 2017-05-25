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
fetchWorkerInputSource eventName workerSpec allInputs =
  let
    inputName =
      wsInputName workerSpec

  in
    maybe (throwIO $ InputNameNotFound eventName inputName)
          return
          (HashMap.lookup inputName allInputs)

createWorker
  :: (EventName, EventSpec)
  -> WorkerSpec
  -> HashMap InputName Input
  -> HashMap EventName (EventSpec, [Output])
  -> (WorkerMsg -> IO ())
  -> IO Worker
createWorker (eventName, eventSpec) workerSpec allInputs outputsPerEvent msgHandler = do
  evInputSource <- fetchWorkerInputSource eventName workerSpec allInputs

  let
    workerEnv =
      WorkerEnv {
        weEventSpec = (eventName, eventSpec)
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

-- workerHandler :: HashMap EventName [SomeEventHandler] -> WorkerMsg -> IO ()
-- workerHandler eventHandlerMap (WorkerMsg env payload deleteMsg) =
--   let
--     evName =
--       fst $ weEventSpec env

--     evSchema =
--       case esSchema $ snd $ weEventSpec env of
--         JSONSchema _ ->
--           SJSON

--     outputMap =
--       weOutputs env

--     handleEvent
--       :: (KnownSymbol evId, IEventHandler schema evId, IEvent (Event evId))
--       => SSchema schema -> Proxy evId -> SomeEventHandler -> IO [SomeOutputEvent]
--     handleEvent schema inputProxy (SomeEventHandler otherSchema handlerProxy) =
--       if isJust (sameSymbol handlerProxy inputProxy) then
--         case unsafeCoerce $ _parseEvent schema handlerProxy payload of
--           Nothing -> do
--             putStrLn $ "WARNING: Expecting Event " <> evName <> " to have a handler, but didn't"
--             return []
--           Just event ->
--             _handleEvent schema handlerProxy event
--       else
--         return []

--   in
--     case someSymbolVal (Text.unpack evName) of
--       SomeSymbol inputProxy ->
--         case HashMap.lookup evName eventHandlerMap of
--           Nothing ->
--             return ()
--           Just handlers ->
--             forM_ handlers $ \handler -> do
--               outputEvents <- handleEvent evSchema inputProxy handler
--               mapM_ (_emitEvent outputMap evSchema) outputEvents
