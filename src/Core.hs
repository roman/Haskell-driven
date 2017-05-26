{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Core where

import Protolude

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Config
import Input
import Output
import Worker

data DrivenRuntime
  = DrivenRuntime
    {
      runtimeInputs :: HashMap InputName Input
    , runtimeEventWorkers :: HashMap EventName [Worker]
    }


startSystem
  :: DrivenConfig
  -> HashMap EventName [SomeEventHandler]
  -> IO DrivenRuntime
startSystem drivenConfig eventHandlers =
  let

    inputStep acc inputSpec = do
      input <- createInput inputSpec
      return $ HashMap.insert (isName inputSpec) input acc

    outputStep inputMap acc outputSpec = do
      output <- createOutput inputMap outputSpec
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
