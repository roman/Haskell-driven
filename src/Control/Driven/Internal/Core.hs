{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Driven.Internal.Core where

import Protolude

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import qualified Control.Driven.Internal.Backend.Memory as Backend
-- import           Control.Driven.Internal.Worker

import           Control.Driven.Internal.Types

--------------------------------------------------------------------------------

data DrivenRuntime
  = DrivenRuntime
    {
      runtimeInputs       :: HashMap InputName Input
    , runtimeOutputs      :: HashMap OutputName Output
    , runtimeEventWorkers :: HashMap EventName [Worker]
    }

--------------------------------------------------------------------------------

collectOutputsPerEventName
  :: HashMap EventName EventSpec
  -> HashMap OutputName Output
  -> IO (HashMap EventName (EventSpec, [Output]))
collectOutputsPerEventName eventSpecMap allOutputs =
  let
    step acc (evName, eventSpec) = do
      outputs <-
        forM (esOutputNames eventSpec) $ \outputName ->
          maybe (throwIO $ OutputNameNotFound evName outputName)
                return
                (HashMap.lookup outputName allOutputs)
      return $ HashMap.insert evName (eventSpec, outputs) acc

  in
    foldM step HashMap.empty (HashMap.toList eventSpecMap)

createSchemaMap
  :: [SchemaSpec]
  -> HashMap EventName EventSpec
  -> IO (HashMap EventName Schema)
createSchemaMap schemaSpecList eventSpecMap  =
  let
    step acc (evName, eventSpec) = do
      result0 <- mapM ($ eventSpec) schemaSpecList

      let
        result =
          catMaybes result0

      case result of
        [] ->
          throwIO $ SchemaForEventNotFound evName (esSchema eventSpec)
        (schema:_) ->
          return $ HashMap.insert evName schema acc
  in
    foldM step HashMap.empty (HashMap.toList eventSpecMap)

createOutputMap
  :: (DrivenEvent -> IO ())
  -> HashMap BackendName Backend
  -> HashMap InputName Input
  -> [OutputSpec]
  -> IO (HashMap OutputName Output)
createOutputMap emitEvent backendMap inputMap outputSpecList =
  let
    step acc outputSpec =
      case HashMap.lookup (osBackendName outputSpec) backendMap of
        Nothing ->
          throwIO $ BackendNameNotFound (osBackendName outputSpec)
        Just backend -> do
          output <- createOutput backend emitEvent inputMap outputSpec
          return $ HashMap.insert (osName outputSpec) output acc
  in
    foldM step HashMap.empty outputSpecList

createInputMap
  :: (DrivenEvent -> IO ())
  -> HashMap BackendName Backend
  -> [InputSpec]
  -> IO (HashMap BackendName Input)
createInputMap emitEvent backendMap inputSpecList =
  let
    step acc inputSpec =
      case HashMap.lookup (isBackendName inputSpec) backendMap of
        Nothing ->
          throwIO $ BackendNameNotFound (isBackendName inputSpec)
        Just backend -> do
          input <- createInput backend emitEvent inputSpec
          return $ HashMap.insert (isName inputSpec) input acc
  in
    foldM step HashMap.empty inputSpecList

createWorkersPerEvent
  :: (DrivenEvent -> IO ())
  -> HashMap EventName EventSpec
  -> HashMap EventName Schema
  -> HashMap InputName Input
  -> HashMap EventName (EventSpec, [Output])
  -> HashMap EventName [SomeEventHandler]
  -> IO (HashMap EventName [Worker])
createWorkersPerEvent emitEvent eventSpecMap schemaMap inputMap outputPerEvent eventHandlers =
  let
    createWorker' evName evSpec =
          createWorker
            emitEvent
            evName
            evSpec
            inputMap
            outputPerEvent
            (workerHandler schemaMap eventHandlers)
  in do
    workersPerEventList <-
      forM (HashMap.toList eventSpecMap) $ \(evName, evSpec) -> do
        workers <- mapM (createWorker' evName evSpec) (esWorkerSpecs evSpec)
        return (evName, workers)

    return $ HashMap.fromList workersPerEventList


startSystem
  :: DrivenConfig
  -> (DrivenEvent -> IO ())
  -> HashMap Text Backend
  -> [SchemaSpec]
  -> HashMap EventName [SomeEventHandler]
  -> IO DrivenRuntime
startSystem drivenConfig emitEvent backendMap0 schemaSpecList eventHandlers = do
  let
    backendMap =
      HashMap.union
         backendMap0
         [("memory_queue", Backend.memoryBackend)]

    inputSpecMap =
      drivenInputs drivenConfig

    outputSpecMap =
      drivenOutputs drivenConfig

    eventSpecMap =
      drivenEvents drivenConfig

  inputMap  <- createInputMap emitEvent backendMap inputSpecMap
  outputMap <- createOutputMap emitEvent backendMap inputMap outputSpecMap
  schemaMap <- createSchemaMap schemaSpecList eventSpecMap

  outputPerEvent <- collectOutputsPerEventName eventSpecMap outputMap
  workersPerEvent <-
    createWorkersPerEvent emitEvent eventSpecMap schemaMap inputMap outputPerEvent eventHandlers

  return $ DrivenRuntime inputMap outputMap workersPerEvent

--
-- stopSystem :: DrivenRuntime -> IO ()
-- stopSystem (DrivenRuntime inputMap outputMap workersPerEvent) = do
--   mapM_ disposeInput (HashMap.elems inputMap)
--   mapM_ disposeOutput (HashMap.elems outputMap)
--   mapM_ (mapM_ disposeWorker) (HashMap.elems workersPerEvent)
