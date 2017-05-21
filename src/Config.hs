{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Config where

import Protolude
import Data.Aeson ((.:), (.:?))
import Data.HashMap.Strict (HashMap)
import Data.Text (isSuffixOf)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)

type EventName = Text

data EventSchema
  = JSONSchema Text
  deriving (Generic, Show, Eq)

data InputQueueType
  = Memory { iqMaxSize :: Int }
  deriving (Generic, Show, Eq)

data InputQueue
  = InputQueue {
      iqType        :: InputQueueType
    , iqWorkerCount :: Int
    }
  deriving (Generic, Show, Eq)

data OutputTopic
  = OutputTopic
  deriving (Generic, Show, Eq)

data EventSpec =
  EventSpec {
    esInputQueue   :: [InputQueue]
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
  }
  deriving (Generic, Show)

instance JSON.FromJSON EventSchema where
  parseJSON value =
    case value of
      JSON.String name
        | ".json" `isSuffixOf` name ->
          return (JSONSchema name)

        | otherwise ->
          JSON.typeMismatch "Paseo.EventSchema" value

      _ ->
        JSON.typeMismatch "Paseo.EventSchema" value

instance JSON.FromJSON OutputTopic where
  parseJSON _ =
    return OutputTopic

parseMemoryInput :: HashMap Text JSON.Value -> JSON.Parser InputQueueType
parseMemoryInput obj =
  Memory <$> obj .: "max_size"

instance JSON.FromJSON InputQueue where
  parseJSON value =
    case value of
      JSON.Object obj -> do
        queueTy <- obj .: "type"
        if queueTy == ("memory" :: Text) then
          InputQueue
            <$> parseMemoryInput obj
            <*> obj .: "worker_count"
        else
          JSON.typeMismatch "Paseo.InputQueue" value

      _ ->
        JSON.typeMismatch "Paseo.InputQueue" value

instance JSON.FromJSON EventSpec where
  parseJSON value =
    case value of
      JSON.Object obj ->
        EventSpec
          <$> obj .: "input"
          <*> (fromMaybe [] <$> obj .:? "output")
          <*> obj .: "schema"
      _ ->
        JSON.typeMismatch "Paseo.EventSpec" value

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
          JSON.typeMismatch "Paseo.Config" value

toWorkerSpecs :: Config -> [WorkerSpec]
toWorkerSpecs (Config entries) =
  -- Derive a worker per input
  let
    step eventName eventSpec allWorkers =
      let
        workers = do
          inputQueue <- esInputQueue eventSpec
          return $ WorkerSpec inputQueue eventName (esEventSchema eventSpec)
      in
        workers <> allWorkers
  in
    HashMap.foldrWithKey step [] entries
