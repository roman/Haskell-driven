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

import Data.Aeson ((.:), (.:?))
import Data.HashMap.Strict (HashMap)
import GHC.TypeLits (SomeSymbol, Symbol, KnownSymbol, symbolVal, sameSymbol)

import Data.Text (isSuffixOf)

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)

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
  = NoSchema
  | JSONSchema { schemaUri :: Text }
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
  = JSON

data SSchema (s :: Schema) where
  SJSON :: SSchema 'JSON

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

type family EventInputConstraint (schema :: Schema) :: * -> Constraint where
  EventInputConstraint 'JSON = JSON.FromJSON

type family EventOutputConstraint (schema :: Schema) :: * -> Constraint where
  EventOutputConstraint 'JSON = JSON.ToJSON

class IEvent ev where
  eventName :: ev -> Text

class ( IEvent ev
      , EventOutputConstraint schema ev )
  => IOutputEventEmitter schema ev where
  _emitEvent
    :: HashMap EventName (EventSpec, [Output])
    -> SSchema schema
    -> ev
    -> IO ()

instance ( IEvent ev
         , JSON.ToJSON ev )
  => IOutputEventEmitter 'JSON ev where
  _emitEvent outputMap SJSON event =
    case HashMap.lookup (eventName event) outputMap of
      Nothing ->
        return ()
      Just (eventSpec, outputs)  ->
        case esSchema eventSpec of
          JSONSchema _jsonSchema ->
            let
              serializedOutput = LBS.toStrict (JSON.encode event)
            in
              mapM_ (flip writeToOutput serializedOutput) outputs
          _ ->
            return ()

data SomeOutputEvent
  = forall ev. ( IEvent ev
               , IOutputEventEmitter 'JSON ev
               ) =>
      SomeOutputEvent ev

instance IEvent SomeOutputEvent where
  eventName (SomeOutputEvent ev) =
    eventName ev

instance JSON.ToJSON SomeOutputEvent where
  toJSON (SomeOutputEvent ev) =
    JSON.toJSON ev

class (EventInputConstraint schema (Event evId))
  => IEventHandler schema evId where

  type Event (evId :: Symbol) :: *

  _parseEvent
    :: SSchema schema
    -> Proxy evId
    -> ByteString
    -> Maybe (Event evId)
  _parseEvent SJSON _ =
    JSON.decodeStrict

  _handleEvent
    :: SSchema schema
    -> Proxy evId
    -> Event evId
    -> IO [SomeOutputEvent]

data SomeEventHandler
  = forall schema evId. (KnownSymbol evId, IEventHandler schema evId) =>
    SomeEventHandler (SSchema schema) (Proxy evId)

--------------------------------------------------------------------------------

parseSchemaSpec
  :: JSON.Value
  -> JSON.Parser SchemaSpec
parseSchemaSpec value =
  case value of
    JSON.String uri
      | ".json" `isSuffixOf` uri ->
        return (JSONSchema uri)
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
      <$> (fromMaybe NoSchema <$> eventObj .:? "schema")
      <*> eventObj .: "workers"
      <*> (fromMaybe [] <$> eventObj .: "outputs")
    _ ->
      JSON.typeMismatch "Driven.EventSpec" value

instance JSON.FromJSON EventSpec where
  parseJSON =
    parseEventSpec
