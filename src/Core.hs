{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Core where

import Protolude

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Config
import qualified IO.Memory as Backend
import Worker

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
collectOutputsPerEventName allEvents allOutputs =
  let
    step acc (evName, eventSpec) = do
      outputs <-
        forM (esOutputNames eventSpec) $ \outputName -> do
          maybe (throwIO $ OutputNameNotFound evName outputName)
                return
                (HashMap.lookup outputName allOutputs)
      return $ HashMap.insert evName (eventSpec, outputs) acc

  in
    foldM step HashMap.empty (HashMap.toList allEvents)


createOutputMap
  :: DrivenConfig
  -> (DrivenEvent -> IO ())
  -> HashMap BackendName Backend
  -> HashMap InputName Input
  -> IO (HashMap OutputName Output)
createOutputMap drivenConfig emitEvent backendMap inputMap =
  let
    step acc outputSpec =
      case HashMap.lookup (osBackendName outputSpec) backendMap of
        Nothing ->
          throwIO $ BackendNameNotFound (osBackendName outputSpec)
        Just backend -> do
          output <- createOutput backend emitEvent inputMap outputSpec
          return $ HashMap.insert (osName outputSpec) output acc
  in
    foldM step HashMap.empty (drivenOutputs drivenConfig)


createInputMap
  :: DrivenConfig
  -> (DrivenEvent -> IO ())
  -> HashMap BackendName Backend
  -> IO (HashMap BackendName Input)
createInputMap drivenConfig emitEvent backendMap =
  let
    step acc inputSpec =
      case HashMap.lookup (isBackendName inputSpec) backendMap of
        Nothing ->
          throwIO $ BackendNameNotFound (isBackendName inputSpec)
        Just backend -> do
          input <- createInput backend emitEvent inputSpec
          return $ HashMap.insert (isName inputSpec) input acc
  in
    foldM step HashMap.empty (drivenInputs drivenConfig)


createWorkersPerEvent
  :: DrivenConfig
  -> (DrivenEvent -> IO ())
  -> HashMap InputName Input
  -> HashMap EventName (EventSpec, [Output])
  -> HashMap EventName [SomeEventHandler]
  -> IO (HashMap EventName [Worker])
createWorkersPerEvent drivenConfig emitEvent inputMap outputPerEvent eventHandlers =
  let
    createWorker' evName evSpec =
          createWorker
            emitEvent
            evName
            evSpec
            inputMap
            outputPerEvent
            (workerHandler eventHandlers)
  in do
    workersPerEventList <-
      forM (HashMap.toList $ drivenEvents drivenConfig) $ \(evName, evSpec) -> do
        workers <- mapM (createWorker' evName evSpec) (esWorkerSpecs evSpec)
        return (evName, workers)

    return $ HashMap.fromList workersPerEventList


startSystem
  :: DrivenConfig
  -> (DrivenEvent -> IO ())
  -> HashMap Text Backend
  -> HashMap EventName [SomeEventHandler]
  -> IO DrivenRuntime
startSystem drivenConfig emitEvent backendMap0 eventHandlers =
  let
    backendMap =
      HashMap.union
         backendMap0
         [("memory_queue", Backend.memoryBackend)]

  in do
    inputMap  <- createInputMap drivenConfig emitEvent backendMap
    outputMap <- createOutputMap drivenConfig emitEvent backendMap inputMap
    outputPerEvent <- collectOutputsPerEventName (drivenEvents drivenConfig) outputMap
    workersPerEvent <-
      createWorkersPerEvent drivenConfig emitEvent inputMap outputPerEvent eventHandlers

    return $ DrivenRuntime inputMap outputMap workersPerEvent

stopSystem :: DrivenRuntime -> IO ()
stopSystem (DrivenRuntime inputMap outputMap workersPerEvent) = do
  mapM_ disposeInput (HashMap.elems inputMap)
  mapM_ disposeOutput (HashMap.elems outputMap)
  mapM_ (mapM_ disposeWorker) (HashMap.elems workersPerEvent)
