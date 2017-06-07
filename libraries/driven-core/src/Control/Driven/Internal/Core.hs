{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Driven.Internal.Core where

import Protolude

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Control.Driven.Internal.Worker

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

processEventDelivers
  :: HashMap InputEventName EventSpec
  -> HashMap OutputName Output
  -> IO (HashMap InputEventName [DeliverySpec], HashMap OutputEventName [Output])
processEventDelivers eventSpecMap outputMap =
  let
    outputEvStep
      :: InputEventName
      -> HashMap OutputEventName [Output]
      -> DeliverySpec
      -> IO (HashMap OutputEventName [Output])
    outputEvStep inputEvName outputMapAcc deliverySpec =
      let
        outputEvName =
          dsEventName deliverySpec

        outputName =
          dsOutputName deliverySpec
      in do
        output <-
            maybe (throwIO $ OutputNameNotFound inputEvName outputEvName outputName)
                  return
                  (HashMap.lookup outputName outputMap)

        return
          $ HashMap.unionWith
              (++)
              (HashMap.singleton outputEvName [output])
              outputMapAcc

    inputEvStep
      :: (HashMap InputEventName [DeliverySpec], HashMap OutputEventName [Output])
      -> (InputEventName, EventSpec)
      -> IO (HashMap InputEventName [DeliverySpec], HashMap OutputEventName [Output])
    inputEvStep (deliverySpecList, outputMapAcc) (inputEvName, eventSpec) = do
      let
        deliveryPerInputEventMap1 =
          HashMap.insert inputEvName
                         (esDeliveries eventSpec)
                         deliverySpecList

      outputMapAcc1 <-
        foldM (outputEvStep inputEvName)
              outputMapAcc
              (esDeliveries eventSpec)

      return $ (deliveryPerInputEventMap1, outputMapAcc1)

  in
    foldM inputEvStep (HashMap.empty, HashMap.empty) (HashMap.toList eventSpecMap)

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
  -> [Transport]
  -> [OutputSpec]
  -> IO (HashMap OutputName Output)
createOutputMap emitDrivenEvent inputMap transportList outputSpecList =
  let
    step acc outputSpec = do
      result0 <-
        mapM (\transport -> createOutput transport emitDrivenEvent inputMap outputSpec)
             transportList

      let
        result =
          catMaybes result0

      case result of
        [] ->
          throwIO $ TransportNameNotFound (osName outputSpec)
        (output:_) ->
          return $ HashMap.insert (osName outputSpec) output acc

  in
    foldM step HashMap.empty outputSpecList

createInputMap
  :: (DrivenEvent -> IO ())
  -> [Transport]
  -> [InputSpec]
  -> IO (HashMap TransportName Input)
createInputMap emitDrivenEvent transportList inputSpecList =
  let
    step acc inputSpec = do
      result0 <-
        mapM (\transport -> createInput transport emitDrivenEvent inputSpec)
            transportList

      let
        result =
          catMaybes result0

      case result of
        [] ->
          throwIO $ TransportNameNotFound (isName inputSpec)
        (input:_) ->
          return $ HashMap.insert (isName inputSpec) input acc

  in
    foldM step HashMap.empty inputSpecList

createWorkersPerEvent
  :: (DrivenEvent -> IO ())
  -> HashMap EventName EventSpec
  -> HashMap EventName Schema
  -> HashMap InputName Input
  -> HashMap EventName [Output]
  -> HashMap EventName [DeliverySpec]
  -> HashMap EventName [SomeEventHandler]
  -> IO (HashMap EventName [Worker])
createWorkersPerEvent emitDrivenEvent eventSpecMap schemaMap inputMap outputPerEvent deliveriesPerInputEvent eventHandlers =
  let
    createWorker' evName evSpec workerSpec =
          createWorker
            emitDrivenEvent
            evName
            evSpec
            workerSpec
            inputMap
            outputPerEvent
            (workerHandler schemaMap deliveriesPerInputEvent eventHandlers)
  in do
    workersPerEventList <-
      forM (HashMap.toList eventSpecMap) $ \(evName, evSpec) -> do
        workers <- mapM (createWorker' evName evSpec) (esWorkerSpecs evSpec)
        return (evName, workers)

    return $ HashMap.fromList workersPerEventList

startSystem
  :: DrivenConfig
  -> (DrivenEvent -> IO ())
  -> [Transport]
  -> [SchemaSpec]
  -> HashMap EventName [SomeEventHandler]
  -> IO DrivenRuntime
startSystem drivenConfig emitDrivenEvent transportList schemaSpecList eventHandlers = do
  let

    inputSpecMap =
      drivenInputs drivenConfig

    outputSpecMap =
      drivenOutputs drivenConfig

    eventSpecMap =
      drivenEvents drivenConfig

  inputMap  <- createInputMap emitDrivenEvent transportList inputSpecMap
  outputMap <- createOutputMap emitDrivenEvent inputMap transportList outputSpecMap
  schemaMap <- createSchemaMap schemaSpecList eventSpecMap

  (outputEventsPerInput, outputPerEvent) <-
    processEventDelivers eventSpecMap outputMap

  workersPerEvent <-
    createWorkersPerEvent
      emitDrivenEvent
      eventSpecMap
      schemaMap
      inputMap
      outputPerEvent
      outputEventsPerInput
      eventHandlers

  return $ DrivenRuntime inputMap outputMap workersPerEvent

stopSystem :: DrivenRuntime -> IO ()
stopSystem (DrivenRuntime inputMap outputMap workersPerEvent) = do
  mapM_ disposeInput (HashMap.elems inputMap)
  mapM_ disposeOutput (HashMap.elems outputMap)
  mapM_ (mapM_ disposeWorker) (HashMap.elems workersPerEvent)
