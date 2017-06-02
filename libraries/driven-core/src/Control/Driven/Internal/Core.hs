{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Driven.Internal.Core where

import Protolude

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import           Control.Driven.Internal.Worker

import Control.Driven.Internal.Types

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
          throwIO $ InvalidSchemaTypeForEvent evName (esSchema eventSpec)
        (schema:_) ->
          return $ HashMap.insert evName schema acc
  in
    foldM step HashMap.empty (HashMap.toList eventSpecMap)

createOutputMap
  :: (DrivenEvent -> IO ())
  -> HashMap InputName Input
  -> [Backend]
  -> [OutputSpec]
  -> IO (HashMap OutputName Output)
createOutputMap emitDrivenEvent inputMap backendList outputSpecList =
  let
    step acc outputSpec = do
      result0 <-
        mapM (\backend -> createOutput backend emitDrivenEvent inputMap outputSpec)
             backendList

      let
        result =
          catMaybes result0

      case result of
        [] ->
          throwIO $ BackendNameNotFound (osName outputSpec)
        (output:_) -> do
          return $ HashMap.insert (osName outputSpec) output acc

  in
    foldM step HashMap.empty outputSpecList

createInputMap
  :: (DrivenEvent -> IO ())
  -> [Backend]
  -> [InputSpec]
  -> IO (HashMap BackendName Input)
createInputMap emitDrivenEvent backendList inputSpecList =
  let
    step acc inputSpec = do
      result0 <-
        mapM (\backend -> createInput backend emitDrivenEvent inputSpec)
            backendList

      let
        result =
          catMaybes result0

      case result of
        [] ->
          throwIO $ BackendNameNotFound (isName inputSpec)
        (input:_) -> do
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
createWorkersPerEvent emitDrivenEvent eventSpecMap schemaMap inputMap outputPerEvent eventHandlers =
  let
    createWorker' evName evSpec workerSpec =
          createWorker
            emitDrivenEvent
            evName
            evSpec
            workerSpec
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
  -> [Backend]
  -> [SchemaSpec]
  -> HashMap EventName [SomeEventHandler]
  -> IO DrivenRuntime
startSystem drivenConfig emitDrivenEvent backendList schemaSpecList eventHandlers = do
  let

    inputSpecMap =
      drivenInputs drivenConfig

    outputSpecMap =
      drivenOutputs drivenConfig

    eventSpecMap =
      drivenEvents drivenConfig

  inputMap  <- createInputMap emitDrivenEvent backendList inputSpecMap
  outputMap <- createOutputMap emitDrivenEvent inputMap backendList outputSpecMap
  schemaMap <- createSchemaMap schemaSpecList eventSpecMap

  outputPerEvent <- collectOutputsPerEventName eventSpecMap outputMap
  workersPerEvent <-
    createWorkersPerEvent emitDrivenEvent eventSpecMap schemaMap inputMap outputPerEvent eventHandlers

  return $ DrivenRuntime inputMap outputMap workersPerEvent

stopSystem :: DrivenRuntime -> IO ()
stopSystem (DrivenRuntime inputMap outputMap workersPerEvent) = do
  mapM_ disposeInput (HashMap.elems inputMap)
  mapM_ disposeOutput (HashMap.elems outputMap)
  mapM_ (mapM_ disposeWorker) (HashMap.elems workersPerEvent)
