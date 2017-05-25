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
module Config where

import Protolude

import Data.Aeson ((.:))
import Data.HashMap.Strict (HashMap)
import GHC.TypeLits (Symbol, KnownSymbol)

import Data.Text (isSuffixOf)

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)
import qualified Data.ProtoLens as Proto (Message, encodeMessage, decodeMessage)
import qualified JSONSchema.Draft4 as D4

--------------------------------------------------------------------------------

type Size = Int
type TimeoutMillis = Int
type InputName = Text
type OutputName = Text
type EventName = Text

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
  = InputMemoryQueueSpec
    { isName         :: InputName
    , isMaxSize      :: Size
    , isRetryAfterMs :: TimeoutMillis
    }
  deriving (Generic, Show, Eq)

data OutputSpec
  = OutputMemoryQueueSpec
    { osName :: OutputName }
  deriving (Generic, Show, Eq)

data DrivenConfig
  = DrivenConfig
    {
      drivenEvents  :: HashMap EventName EventSpec
    , drivenInputs  :: [InputSpec]
    , drivenOutputs :: [OutputSpec]
    }
  deriving (Generic, Show, Eq)

data ConfigError
  = InputNameNotFound EventName InputName
  | OutputNameNotFound EventName OutputName
  deriving (Generic, Show)

instance Exception ConfigError

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
    }

data Output
  = Output
    { writeToOutput :: ByteString -> IO () }

data WorkerEnv
  = WorkerEnv {
    weEventSpec :: (EventName, EventSpec)
  , weOutputs   :: HashMap EventName (EventSpec, [Output])
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
    { cancelWorker  :: IO () }

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
      JSON.typeMismatch "Paseo.WorkerSpec" value

instance JSON.FromJSON WorkerSpec where
  parseJSON =
    parseWorkerSpec

parseInputMemoryQueue
  :: JSON.Value
  -> JSON.Parser InputSpec
parseInputMemoryQueue value =
  case value of
    JSON.Object memoryQueueObj -> do
      InputMemoryQueueSpec
        <$> memoryQueueObj .: "name"
        <*> memoryQueueObj .: "max_size"
        <*> memoryQueueObj .: "retry_after_ms"
    _ ->
      JSON.typeMismatch "Driven.InputSpec" value

instance JSON.FromJSON InputSpec where
  parseJSON =
    parseInputMemoryQueue

parseOutputMemoryQueue
  :: JSON.Value
  -> JSON.Parser OutputSpec
parseOutputMemoryQueue value =
  case value of
    JSON.Object memoryQueueObj -> do
      OutputMemoryQueueSpec
        <$> memoryQueueObj .: "name"
    _ ->
      JSON.typeMismatch "Driven.OutputSpec" value

instance JSON.FromJSON OutputSpec where
  parseJSON =
    parseOutputMemoryQueue

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
  fromProtobuff :: ByteString -> Either [Char] msg

instance Proto.Message msg => FromProtobuff msg where
  fromProtobuff = Proto.decodeMessage

class ToProtobuff msg where
  toProtobuff :: msg -> ByteString

instance Proto.Message msg => ToProtobuff msg where
  toProtobuff = Proto.encodeMessage

type family InputEventConstraint (schema :: Schema) :: * -> Constraint where
  InputEventConstraint 'Json = JSON.FromJSON
  InputEventConstraint 'Proto = FromProtobuff

type family OutputEventConstraint (schema :: Schema) :: * -> Constraint where
  OutputEventConstraint 'Json = JSON.ToJSON
  OutputEventConstraint 'Proto = ToProtobuff

--------------------

class IEvent ev where
  eventName :: ev -> Text

class ( IEvent ev ) => IOutputEventEmitter ev where
  _emitEvent
    :: OutputEventConstraint schema ev
    => SSchema schema
    -> HashMap EventName (EventSpec, [Output])
    -> ev
    -> IO ()
  _emitEvent schema outputMap event =
    case HashMap.lookup (eventName event) outputMap of
      Nothing ->
        return ()
      Just (eventSpec, outputs)  ->
        case (schema, esSchema eventSpec) of
          (SJson, JsonSchema jsonSchemaPath) ->
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
                  panic "JSON serialization doesn't comply with JSON schema"
                Right _ ->
                  mapM_ (flip writeToOutput $ LBS.toStrict (JSON.encode eventJson))
                        outputs

          (SProto, Protobuffer) ->
            let
              serializedOutput =
                toProtobuff event
            in
              mapM_ (flip writeToOutput serializedOutput) outputs

          (_codeConstraint, configConstraint) ->
            panic
            $ "ERROR: Expecting " <> show configConstraint
            <> " but got different constraint on code"

data SomeOutputEvent =
  forall schema ev. (OutputEventConstraint schema ev, IOutputEventEmitter ev) =>
    SomeOutputEvent (SSchema schema) ev

instance IEvent SomeOutputEvent where
  eventName (SomeOutputEvent _ ev) =
    eventName ev

instance JSON.ToJSON SomeOutputEvent where
  toJSON (SomeOutputEvent innerSchema ev) =
    case innerSchema of
      SJson ->
        JSON.toJSON ev
      _ ->
        panic "Internal OutputEvent is not JSON serializable"

instance ToProtobuff SomeOutputEvent where
  toProtobuff (SomeOutputEvent innerSchema ev) =
    case innerSchema of
      SProto ->
        toProtobuff ev
      _ ->
        panic "Internal OutputEvent is not Protobuff serializable"

instance IOutputEventEmitter SomeOutputEvent where
  _emitEvent _schema outputMap (SomeOutputEvent innerSchema ev) =
    _emitEvent innerSchema outputMap ev


--------------------
-- Event Handler (Input)

class IEventHandler evId where
  type Event (evId :: Symbol) :: *

  _parseEvent
    :: (InputEventConstraint schema (Event evId))
    => SSchema schema
    -> Proxy evId
    -> ByteString
    -> Maybe (Event evId)
  _parseEvent schema _ bs =
    case schema of
      SJson ->
        JSON.decodeStrict bs
      SProto ->
        either (const Nothing) Just (fromProtobuff bs)

  _handleEvent
    :: Proxy evId
    -> Event evId
    -> IO [SomeOutputEvent]

data SomeEventHandler
  = forall schema evId. (KnownSymbol evId,
                         IEventHandler evId,
                         InputEventConstraint schema (Event evId)) =>
    SomeEventHandler (SSchema schema) (Proxy evId)
