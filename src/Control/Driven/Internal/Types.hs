{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Driven.Internal.Types where

import Protolude

import Data.Aeson ((.:))
import Data.HashMap.Strict (HashMap)

import Data.Text (isSuffixOf)

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, camelTo2, constructorTagModifier, typeMismatch, fieldLabelModifier, defaultTaggedObject, tagFieldName, sumEncoding)
import qualified Data.ProtoLens as Proto (Message, encodeMessage, decodeMessage)
import qualified JSONSchema.Draft4 as D4

--------------------------------------------------------------------------------

type Size = Int
type TimeoutMillis = Int
type InputName = Text
type OutputName = Text
type EventName = Text
type TypeName = Text
type ErrorMessage = Text
type BackendName = Text
type FormatName = Text
type EventPayload = Text

data WorkerSpec
  = WorkerSpec
    { wsInputName :: InputName
    , wsTimeout   :: Int
    , wsCount     :: Int
    }
  deriving (Generic, Show, Eq)

data SchemaSpec
  = JsonSchema { schemaUri :: Text }
  | Protobuffer
  deriving (Generic, Show, Eq)

data EventSpec
  = EventSpec
    { esSchema       :: SchemaSpec
    , esWorkerSpecs  :: [WorkerSpec]
    , esOutputNames  :: [OutputName]
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
    { osName          :: OutputName
    , osBackendName   :: BackendName
    , osObject        :: HashMap Text JSON.Value
    , osDrivenObject  :: HashMap Text JSON.Value
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
      JSON.Object configSpecObj -> do
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
  | BackendNameNotFound BackendName
  deriving (Generic, Show)

instance Exception DrivenError

data DrivenEvent
  = InputCreated { deBackendName :: BackendName, deInputName :: InputName }
  | InputDisposed { deBackendName :: BackendName, deInputName :: InputName }
  | OutputCreated { deBackendName :: BackendName, deOutputName :: OutputName }
  | OutputDisposed { deBackendName :: BackendName, deOutputName :: OutputName }
  | EventWorkerCreated { deInputName :: InputName, deEventName :: EventName }
  | EventWorkerDisposed { deInputName :: InputName, deEventName :: EventName }
  | EventReceived { deInputName :: InputName, deEventName :: EventName, deFormatName :: FormatName }
  | InvalidEntryIgnored
      {
        deInputName :: InputName
      , deEventName :: EventName
      , deFormatName :: FormatName
      , deEventPayload :: EventPayload
      }
  | EventFormatMissconfigured { deInputName :: InputName, deEventName :: EventName }
  | EventHandlerMissconfigured { deInputName :: InputName, deEventName :: EventName }
  | EventHandlerFailed { deInputName :: InputName, deEventName :: EventName, deMessage :: ErrorMessage }
  deriving (Generic, Show)


drivenEventLabelModifier :: [Char] -> [Char]
drivenEventLabelModifier =
  JSON.camelTo2 '_' . drop 2

instance JSON.ToJSON DrivenEvent where
  toEncoding =
    let
      options =
        JSON.defaultOptions {
          JSON.fieldLabelModifier = drivenEventLabelModifier
        , JSON.constructorTagModifier = JSON.camelTo2 '_'
        , JSON.sumEncoding = (JSON.defaultTaggedObject { JSON.tagFieldName = "type" })
        }
    in
      JSON.genericToEncoding options

--------------------

data Schema
  = Json
  | Proto

data SSchema (s :: Schema) where
  SJson  :: SSchema 'Json
  SProto :: SSchema 'Proto

data Input
  = Input
    {
      readFromInput   :: IO (ByteString, IO ())
    , writeToInput    :: ByteString -> IO ()
    , disposeInput    :: IO ()
    }

data Output
  = Output
    { writeToOutput :: ByteString -> IO ()
    , disposeOutput :: IO () }

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
    weEventSpec :: (EventName, EventSpec)
  , weOutputs   :: HashMap EventName (EventSpec, [Output])
  , weInputName :: InputName
  , weEmitEvent :: DrivenEvent -> IO ()
  }

data WorkerMsg
  = WorkerMsg
    {
      wmWorkerEnv :: WorkerEnv
    , wmPayload   :: ByteString
    , wmDeleteMsg :: IO ()
    }

data Worker
  = Worker
    { disposeWorker :: IO () }

--------------------------------------------------------------------------------

parseSchemaSpec
  :: JSON.Value
  -> JSON.Parser SchemaSpec
parseSchemaSpec value =
  case value of
    JSON.String schemaName
      | ".json" `isSuffixOf` schemaName ->
        return (JsonSchema schemaName)
      | schemaName == "protobuffer" ->
        return Protobuffer
      | otherwise ->
        JSON.typeMismatch "Driven.SchemaSpec" value

    _ ->
      JSON.typeMismatch "Driven.SchemaSpec" value

instance JSON.FromJSON SchemaSpec where
  parseJSON =
    parseSchemaSpec

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

class FromProtobuff msg where
  fromProtobuff :: ByteString -> Maybe msg

instance Proto.Message msg => FromProtobuff msg where
  fromProtobuff =
    either (const Nothing) Just . Proto.decodeMessage

class ToProtobuff msg where
  toProtobuff :: msg -> ByteString

instance Proto.Message msg => ToProtobuff msg where
  toProtobuff = Proto.encodeMessage

--------------------

data Msg a
  = Msg { msgDelete  :: IO ()
        , msgPayload :: a }

data SomeOutputEvent
  = forall ev. (IEvent ev, JSON.ToJSON ev) =>
    JsonOutputEvent ev
  | forall ev. (IEvent ev, ToProtobuff ev) =>
    ProtoOutputEvent ev

json :: (IEvent ev, JSON.ToJSON ev) => ev -> SomeOutputEvent
json = JsonOutputEvent

proto :: (IEvent ev, ToProtobuff ev) => ev -> SomeOutputEvent
proto = ProtoOutputEvent

class IEvent ev where
  eventName :: ev -> Text

instance IEvent SomeOutputEvent where
  eventName (JsonOutputEvent ev) =
    eventName ev
  eventName (ProtoOutputEvent ev) =
    eventName ev

_emitEvent
  :: HashMap EventName (EventSpec, [Output])
  -> SomeOutputEvent
  -> IO ()
_emitEvent outputMap someEvent = do
    case HashMap.lookup (eventName someEvent) outputMap of
      Nothing ->
        return ()
      Just (eventSpec, outputs)  ->
        case (someEvent, esSchema eventSpec) of
          (JsonOutputEvent event, JsonSchema jsonSchemaPath) ->
            let
              eventJson =
                JSON.toJSON event

            in do
              schemaJson <-
                (fromMaybe (panic "Invalid JSON format on schema") . JSON.decodeStrict)
                <$> BS.readFile (Text.unpack jsonSchemaPath)
              result <-
                D4.fetchHTTPAndValidate (D4.SchemaWithURI schemaJson Nothing)
                                        eventJson

              case result of
                Left _ ->
                  -- TODO: Change this to a logger
                  putStrLn ("JSON serialization doesn't comply with JSON schema" :: Text)
                Right _ ->
                  mapM_ (flip writeToOutput $ LBS.toStrict (JSON.encode eventJson))
                        outputs

          (ProtoOutputEvent event, Protobuffer) ->
            let
              serializedOutput =
                toProtobuff event
            in
              mapM_ (flip writeToOutput serializedOutput) outputs

          (_codeConstraint, configConstraint) ->
            putStrLn
              $ "ERROR: Expecting " <> show configConstraint
              <> (" but got different constraint on code" :: Text)

--------------------
-- Event Handler (Input)

data SomeEventHandler
  = forall ev. (JSON.FromJSON ev) =>
    JsonEventHandler (Msg ev -> IO [SomeOutputEvent])
  | forall ev. (FromProtobuff ev) =>
    ProtoEventHandler (Msg ev -> IO [SomeOutputEvent])

jsonHandler
  :: (JSON.FromJSON ev)
  => (Msg ev -> IO [SomeOutputEvent])
  -> SomeEventHandler
jsonHandler handler = JsonEventHandler handler

protoHandler
  :: (FromProtobuff ev)
  => (Msg ev -> IO [SomeOutputEvent])
  -> SomeEventHandler
protoHandler handler = ProtoEventHandler handler
