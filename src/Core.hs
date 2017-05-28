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

data DrivenRuntime
  = DrivenRuntime
    {
      runtimeInputs :: HashMap InputName Input
    , runtimeEventWorkers :: HashMap EventName [Worker]
    }

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

startSystem
  :: DrivenConfig
  -> HashMap Text Backend
  -> HashMap EventName [SomeEventHandler]
  -> IO DrivenRuntime
startSystem drivenConfig backendMap0 eventHandlers =
  let
    backendMap =
      HashMap.union backendMap0 [("memory_queue", Backend.memoryBackend)]

    inputStep acc inputSpec =
      case HashMap.lookup (isTypeName inputSpec) backendMap of
        Nothing ->
          -- TODO: input/output backend not found
          error "pending"
        Just backend -> do
          input <- createInput backend inputSpec
          return $ HashMap.insert (isName inputSpec) input acc

    outputStep
      :: HashMap InputName Input
      -> HashMap OutputName Output
      -> OutputSpec
      -> IO (HashMap OutputName Output)
    outputStep inputMap acc outputSpec = do
      case HashMap.lookup (osTypeName outputSpec) backendMap of
        Nothing ->
          -- TODO: input/output backend not found
          error "pending"
        Just backend -> do
          output <- createOutput backend inputMap outputSpec
          return $ HashMap.insert (osName outputSpec) output acc
  in do
    inputMap  <- foldM inputStep HashMap.empty (drivenInputs drivenConfig)
    outputMap <- foldM (outputStep inputMap) HashMap.empty (drivenOutputs drivenConfig)
    outputPerEvent <- collectOutputsPerEventName (drivenEvents drivenConfig) outputMap
    workersPerEventList <-
      forM (HashMap.toList $ drivenEvents drivenConfig) $ \(evName, evSpec) -> do
        workers <-
          forM (esWorkerSpecs evSpec) $ \workerSpec ->
            createWorker
              (evName, evSpec)
              workerSpec
              inputMap
              outputPerEvent
              (workerHandler eventHandlers)
        return (evName, workers)

    let
      workersPerEvent =
        HashMap.fromList workersPerEventList

    return $ DrivenRuntime inputMap workersPerEvent
