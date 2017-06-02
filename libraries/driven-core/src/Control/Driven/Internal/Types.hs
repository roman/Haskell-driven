{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Control.Driven.Internal.Types where

import Protolude

import Data.Aeson          ((.:))
import Data.HashMap.Strict (HashMap)

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
    ( Parser
    , camelTo2
    , constructorTagModifier
    , defaultTaggedObject
    , fieldLabelModifier
    , sumEncoding
    , tagFieldName
    , typeMismatch
    )

--------------------------------------------------------------------------------

type Size = Int
type TimeoutMillis = Int
type InputName = Text
type OutputName = Text
type EventName = Text
type TypeName = Text
type ErrorMessage = Text
type BackendName = Text
type SchemaTypeName = Text
type EventPayload = Text

data WorkerSpec
  = WorkerSpec
    { wsInputName :: InputName
    , wsTimeout   :: Int
    , wsCount     :: Int
    }
  deriving (Generic, Show, Eq)

data EventSpec
  = EventSpec
    { esSchema      :: JSON.Value
    , esWorkerSpecs :: [WorkerSpec]
    , esOutputNames :: [OutputName]
    }
  deriving (Generic, Show, Eq)

data InputSpec
  = InputSpec
    { isName         :: InputName
    , isBackendName  :: BackendName
    , isObject       :: HashMap Text JSON.Value
    , isDrivenObject :: HashMap Text JSON.Value
    }
  deriving (Generic, Show, Eq)

data OutputSpec
  = OutputSpec
    { osName         :: OutputName
    , osBackendName  :: BackendName
    , osObject       :: HashMap Text JSON.Value
    , osDrivenObject :: HashMap Text JSON.Value
    }
  deriving (Generic, Show, Eq)

data DrivenConfig
  = DrivenConfig
    {
      drivenEvents  :: HashMap EventName EventSpec
    , drivenInputs  :: [InputSpec]
    , drivenOutputs :: [OutputSpec]
    }
  deriving (Generic, Show, Eq)

instance JSON.FromJSON DrivenConfig where
  parseJSON value =
    case value of
      JSON.Object configSpecObj ->
        DrivenConfig
          <$> configSpecObj .: "events"
          <*> (configSpecObj .: "inputs"
                >>= mapM (parseInputSpec configSpecObj))
          <*> (configSpecObj .: "outputs"
                >>= mapM (parseOutputSpec configSpecObj))
      _ ->
        JSON.typeMismatch "DrivenConfig" value

data DrivenError
  = InputNameNotFound InputName
  | OutputNameNotFound EventName OutputName
  | InputCreationError InputSpec ErrorMessage
  | OutputCreationError OutputSpec ErrorMessage
  | InvalidSchemaTypeForEvent EventName JSON.Value
  | BackendNameNotFound BackendName
  | EventHandlerInputParserFailed ErrorMessage
  deriving (Generic, Show)

instance Exception DrivenError

data DrivenEvent
  = InputCreated { deBackendName :: BackendName, deInputName :: InputName }
  | InputDisposed { deBackendName :: BackendName, deInputName :: InputName }
  | OutputCreated { deBackendName :: BackendName, deOutputName :: OutputName }
  | OutputDisposed { deBackendName :: BackendName, deOutputName :: OutputName }
  | EventOutputMissconfigured { deEventName :: EventName }
  | EventSchemaMissconfigured { deEventName :: EventName }
  | EventWorkerCreated { deInputName :: InputName, deEventName :: EventName }
  | EventWorkerDisposed { deInputName :: InputName, deEventName :: EventName }
  | EventReceived
      { deInputName      :: InputName
      , deEventName      :: EventName
      , deSchemaTypeName :: SchemaTypeName
      }
  | InvalidEntryIgnored
      { deInputName    :: InputName
      , deEventName    :: EventName
      , deFormatName   :: SchemaTypeName
      , deEventPayload :: EventPayload
      }
  | EventFormatMissconfigured { deInputName :: InputName, deEventName :: EventName }
  | EventHandlerMissconfigured { deInputName :: InputName, deEventName :: EventName }
  | EventHandlerSucceeded
    {
      deInputName    :: InputName
    , deEventName    :: EventName
    , deOutputEvents :: [EventName]
    }
  | EventHandlerFailed
    { deInputName :: InputName
    , deEventName :: EventName
    , deMessage   :: ErrorMessage
    }
  | EventSerializerMissconfigured
    { deEventName        :: EventName
    , deCodeSchema       :: Text
    , deConfiguredSchema :: Text
    }
  | EventSchemaMissmatch
    { deEventName :: EventName
    , deSchema    :: Text
    }
  deriving (Generic, Show)


drivenEventLabelModifier :: [Char] -> [Char]
drivenEventLabelModifier =
  JSON.camelTo2 '_' . drop 2

instance JSON.ToJSON DrivenEvent where
  toEncoding =
    let
      options =
        JSON.defaultOptions {
          JSON.fieldLabelModifier =
            drivenEventLabelModifier

        , JSON.constructorTagModifier =
            JSON.camelTo2 '_'

        , JSON.sumEncoding =
            JSON.defaultTaggedObject { JSON.tagFieldName = "type" }
        }
    in
      JSON.genericToEncoding options

--------------------

data Input
  = Input
    {
      readFromInput :: IO (ByteString, IO ())
    , writeToInput  :: ByteString -> IO ()
    , disposeInput  :: IO ()
    }

data Output
  = Output
    { writeToOutput :: ByteString -> IO ()
    , disposeOutput :: IO () }

data Event
  = Event
    {
      eSpec    :: EventSpec
    , eOutputs :: [Output]
    , eSchema  :: Schema
    }

data
  Backend
  = Backend {
      createInput
        :: (DrivenEvent -> IO ())
        -> InputSpec
        -> IO Input
    , createOutput
        :: (DrivenEvent -> IO ())
        -> HashMap InputName Input
        -> OutputSpec
        -> IO Output
    }

data WorkerEnv
  = WorkerEnv {
    weEventSpec       :: (EventName, EventSpec)
  , weOutputs         :: HashMap EventName (EventSpec, [Output])
  , weInputName       :: InputName
  , weEmitDrivenEvent :: DrivenEvent -> IO ()
  }

data WorkerMsg
  = WorkerMsg
    {
      wmWorkerEnv :: WorkerEnv
    , wmPayload   :: ByteString
    , wmDeleteMsg :: IO ()
    }

newtype Worker
  = Worker
    { disposeWorker :: IO () }

--------------------------------------------------------------------------------

parseWorkerSpec
  :: JSON.Value
  -> JSON.Parser WorkerSpec
parseWorkerSpec value =
  case value of
    JSON.Object workerObj ->
      WorkerSpec
      <$> workerObj .: "input"
      <*> workerObj .: "timeout_ms"
      <*> workerObj .: "count"
    _ ->
      JSON.typeMismatch "Driven.WorkerSpec" value

instance JSON.FromJSON WorkerSpec where
  parseJSON =
    parseWorkerSpec

parseInputSpec
  :: HashMap Text JSON.Value
  -> JSON.Value
  -> JSON.Parser InputSpec
parseInputSpec drivenConfigObj inputSpecValue =
  case inputSpecValue of
    JSON.Object inputSpecObj -> do
      inputTypeName <- inputSpecObj .: "type"
      inputName <- inputSpecObj .: "name"
      return $ InputSpec inputName inputTypeName inputSpecObj drivenConfigObj
    _ ->
      JSON.typeMismatch "Driven.InputSpec" inputSpecValue

parseOutputSpec
  :: HashMap Text JSON.Value
  -> JSON.Value
  -> JSON.Parser OutputSpec
parseOutputSpec drivenConfigObj outputSpecValue =
  case outputSpecValue of
    JSON.Object outputSpecObj -> do
      outputTypeName <- outputSpecObj .: "type"
      outputName <- outputSpecObj .: "name"
      return $ OutputSpec outputName outputTypeName outputSpecObj drivenConfigObj
    _ ->
      JSON.typeMismatch "Driven.OutputSpec" outputSpecValue

parseEventSpec :: JSON.Value -> JSON.Parser EventSpec
parseEventSpec value =
  case value of
    JSON.Object eventObj ->
      EventSpec
      <$> eventObj .: "schema"
      <*> eventObj .: "workers"
      <*> (fromMaybe [] <$> eventObj .: "outputs")
    _ ->
      JSON.typeMismatch "Driven.EventSpec" value

instance JSON.FromJSON EventSpec where
  parseJSON =
    parseEventSpec

--------------------------------------------------------------------------------

class IOutputEvent event where
  eventName :: event -> Text

class IOutputEvent serializer => IOutputSerializer serializer where
  serializeEvent :: serializer -> ByteString

data SomeOutputEvent =
  forall event. IOutputSerializer event
    => SomeOutputEvent event

instance IOutputEvent SomeOutputEvent where
  eventName (SomeOutputEvent event) =
    eventName event

instance IOutputSerializer SomeOutputEvent where
  serializeEvent (SomeOutputEvent event) =
    serializeEvent event

--------------------

class IEventHandler handler where
  handlerTypeName :: handler -> Text
  handleEvent :: handler -> ByteString -> IO (Either SomeException [SomeOutputEvent])

data SomeEventHandler
  = forall handler. IEventHandler handler => SomeEventHandler handler

instance IEventHandler SomeEventHandler where
  handlerTypeName (SomeEventHandler handler) =
    handlerTypeName handler
  handleEvent (SomeEventHandler handler) =
    handleEvent handler

--------------------

type SchemaName
  = Text

type Schema
  = ByteString -> Either SomeException ()

type SchemaSpec
  = EventSpec -> IO (Maybe Schema)
