{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Config where

import Protolude
import Data.Aeson ((.:))
import Data.HashMap.Strict (HashMap)
import Data.Text (isSuffixOf)

import GHC.TypeLits (Symbol)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (typeMismatch)

import qualified JSON as JSONSchema
import qualified Proto as ProtoSchema

type EventName = Text
-- type QueueURL = Text
-- type TopicARN = Text
-- type GCSubscriptionName = Text
-- type GCTopicName = Text

-- data BackendCredentials
--   = Memory
--   | GC
--   | AWS

data EventSchema
  = JSONSchema Text
  -- | Protobuffer
  deriving (Generic, Show, Eq)

data InputQueue
  = InputQueueMemory
  -- | AWSSQS QueueURL
  -- | GCSubscription GCTopicName GCSubscriptionName
  deriving (Generic, Show, Eq)

data OutputTopic
  = OutputTopicMemory
  -- | AWSSNS TopicARN
  -- | GCPublisher GCTopicName
  deriving (Generic, Show, Eq)

data EventSpec =
  EventSpec {
    esInputQueue   :: InputQueue
  , esOutputTopics :: [OutputTopic]
  , esEventSchema  :: EventSchema
  }
  deriving (Generic, Show)

data WorkerSpec =
  WorkerSpec {
    wsInputQueue  :: InputQueue
  , wsEventName   :: EventName
  , wsEventSchema :: EventSchema
  }
  deriving (Generic, Show)

newtype Config =
  Config {
    eventEntries :: HashMap EventName EventSpec
  -- , backendCredentials :: [BackendCredentials]
  }
  deriving (Generic, Show)

instance JSON.FromJSON EventSchema where
  parseJSON value =
    case value of
      JSON.String name
        -- | name == "protobuffer" ->
        --   return Protobuffer
        | name `isSuffixOf` ".json" ->
          return (JSONSchema name)
        | otherwise ->
          JSON.typeMismatch "DidWat.EventSchema" value
      _ ->
        JSON.typeMismatch "DidWat.EventSchema" value

instance JSON.FromJSON OutputTopic where
  parseJSON _ =
    return OutputTopicMemory

instance JSON.FromJSON InputQueue where
  parseJSON _ =
    return InputQueueMemory

instance JSON.FromJSON EventSpec where
  parseJSON value =
    case value of
      JSON.Object obj ->
        EventSpec
          <$> obj .: "input"
          <*> obj .: "output"
          <*> obj .: "schema"
      _ ->
        JSON.typeMismatch "DidWat.EventSpec" value

instance JSON.FromJSON Config where
  parseJSON value =
    let

      step input acc eventName = do
        eventSpec <- input .: eventName
        return $ HashMap.insert eventName eventSpec acc

    in
      case value of
        JSON.Object input ->
          Config
            <$> foldM (step input) HashMap.empty (HashMap.keys input)
        _ ->
          JSON.typeMismatch "DidWat.Config" value


type family GEventCallback (schema :: Symbol) :: *

type instance GEventCallback "json_schema" = JSONSchema.SomeEventHandler
-- type instance GEventCallback "protobuf" = ProtoSchema.SomeEventHandler

newtype EventCallbackRegistry schema
  = ECR (HashMap EventName [GEventCallback schema])

--------------------------------------------------------------------------------

workerSpecs :: Config -> [WorkerSpec]
workerSpecs config =
  let
    step eventName eventSpec acc =
      let
        ws =
          WorkerSpec { wsInputQueue = esInputQueue eventSpec
                     , wsEventName  = eventName
                     , wsEventSchema = esEventSchema eventSpec }
      in
        ws : acc
  in
    HashMap.foldrWithKey step [] (eventEntries config)
