{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Driven.Internal.Worker where

import Protolude

import Control.Concurrent.Async (async, cancel)
import Data.HashMap.Strict      (HashMap)

import qualified Control.Concurrent.MSemN2 as Sem (new, wait, signal,peekAvail)
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap

import Control.Driven.Internal.Types

fetchWorkerInputSource
  :: WorkerSpec
  -> HashMap InputName Input
  -> IO Input
fetchWorkerInputSource workerSpec inputMap =
  let
    inputName =
      wsConsumes workerSpec

  in
    maybe (throwIO $ InputNameNotFound inputName)
          return
          (HashMap.lookup inputName inputMap)

checkEventDeliveryContract
  :: HashMap InputEventName [DeliverySpec]
  -> InputEventName
  -> [SomeOutputEvent]
  -> Either SomeException ()
checkEventDeliveryContract deliverySpecListPerInput inputEventName actualEvents =
  let
    actualOutputEventNameSet =
      Set.fromList (map eventKey actualEvents)

  in
    case HashMap.lookup inputEventName deliverySpecListPerInput of
      Nothing ->
        Left $ toException $ EventDeliveriesConfigurationMissing inputEventName

      Just deliverySpecList ->
        let
          specOutputEventNameSet =
            deliverySpecList
            & concatMap (\deliverySpec ->
                           if dsOptional deliverySpec then
                             []
                           else
                             [dsEventName deliverySpec])
            & Set.fromList

          shouldNotifyError =
            not $ specOutputEventNameSet  `Set.isSubsetOf` actualOutputEventNameSet

          missingOutputEventNameList =
            Set.toList
              $ Set.difference specOutputEventNameSet actualOutputEventNameSet
        in
          if shouldNotifyError then
            Left
              $ toException
              $ EventHandlerDeliveriesMissing inputEventName missingOutputEventNameList
          else
            Right ()

workerHandler
  :: HashMap InputEventName Schema
  -> HashMap InputEventName [DeliverySpec]
  -> HashMap InputEventName [SomeEventHandler]
  -> WorkerMsg
  -> IO ()
workerHandler schemaMap deliveriesPerInputEvent eventHandlerMap (WorkerMsg env inputBytes deleteMsg) =
  let
    inputEventName =
      fst $ weEventSpec env

    inputName =
      weInputName env

    outputMap =
      weOutputs env

    emitOutputEvent outputEvent =
      let
        outputEventName =
          eventKey outputEvent

        mOutputs =
          HashMap.lookup outputEventName outputMap

      in case mOutputs of
        Nothing ->
          weEmitDrivenEvent env (EventOutputMissconfigured inputEventName)

        Just outputList ->
          forM_ outputList $ \output ->
            case HashMap.lookup outputEventName schemaMap of
              Nothing ->
                weEmitDrivenEvent env (EventSchemaMissconfigured inputEventName)
              Just checkSchema ->
                let
                  outputBytes =
                    serializeEvent outputEvent
                in
                  case checkSchema outputBytes of
                    Left err ->
                      weEmitDrivenEvent env (EventSchemaMissmatch outputEventName (show err))

                    Right _ ->
                      output `writeToOutput` outputBytes

    runEventHandler :: SomeEventHandler -> IO ()
    runEventHandler someHandler = do
      weEmitDrivenEvent env (EventReceived inputName inputEventName (handlerTypeName someHandler))
      handlerResult <- try $ handleEvent someHandler inputBytes
      case handlerResult of
        -- Failure from the actual Handler
        Left (err :: SomeException) ->
          weEmitDrivenEvent env (EventHandlerFailed inputName inputEventName $ show err)

        -- Failure from the Schema implementation
        Right (Left err) ->
          weEmitDrivenEvent env (EventHandlerFailed inputName inputEventName $ show err)

        Right (Right outputEventList) -> do
          case checkEventDeliveryContract deliveriesPerInputEvent inputEventName outputEventList of
            Left err ->
              weEmitDrivenEvent env (EventHandlerFailed inputName inputEventName $ show err)

            Right _ -> do
              mapM_ emitOutputEvent outputEventList
              deleteMsg
              weEmitDrivenEvent env
                (EventHandlerSucceeded
                  inputName inputEventName $ map eventKey outputEventList)


  in
    case HashMap.lookup inputEventName eventHandlerMap of
      Nothing ->
        weEmitDrivenEvent env $ EventHandlerMissconfigured (weInputName env) inputEventName

      Just handlers ->
        mapM_ runEventHandler handlers

createWorker
  :: (DrivenEvent -> IO ())
  -> InputEventName
  -> EventSpec
  -> WorkerSpec
  -> HashMap InputName Input
  -> HashMap OutputEventName [Output]
  -> (WorkerMsg -> IO ())
  -> IO Worker
createWorker emitDrivenEvent evName evSpec workerSpec inputMap outputsPerEvent msgHandler = do
  evInputSource <- fetchWorkerInputSource workerSpec inputMap

  let
    inputName =
      wsConsumes workerSpec

    workerEnv =
      WorkerEnv {
        weEventSpec = (evName, evSpec)
      , weOutputs   = outputsPerEvent
      , weInputName = inputName
      , weEmitDrivenEvent = emitDrivenEvent
      }

    callHandler (message, deleteMsg) =
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

  workerSemaphore <- Sem.new (wsCount workerSpec)
  workerLoop <-
    async $ do
      emitDrivenEvent (EventWorkerCreated evName inputName)
      forever $ do
        availableWorkers <- Sem.peekAvail workerSemaphore
        messageList <- readFromInput evInputSource (min 1 availableWorkers)
        forM messageList $ \message ->
          bracket (Sem.wait workerSemaphore 1)
                  (const $ Sem.signal workerSemaphore 1)
                  (const $ void $ callHandler message)

  return (Worker $ cleanupWorker workerLoop)
