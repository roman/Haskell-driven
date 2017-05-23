{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Config where

import GHC.TypeLits (SomeSymbol(..), Symbol, KnownSymbol, someSymbolVal, sameSymbol)

import Protolude
import Data.Aeson ((.:), (.:?))
import Data.HashMap.Strict (HashMap)
import Data.Text (isSuffixOf)

import Data.Hashable (Hashable)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, typeMismatch)

--------------------------------------------------------------------------------

type EventName = Text

data WorkerSpec =
  WorkerSpec {
    wsTimeout :: Int
  , wsCount   :: Int
  }
  deriving (Generic, Show, Eq, Hashable)

data InputSpec
  = MemoryQueueSpec
    { iqMaxSize      :: Int
    , iqWorkerSpec   :: WorkerSpec
    , iqRetryAfterMs :: Int
    }
  deriving (Generic, Show, Eq, Hashable)


data OutputSpec
  = OutputSpec
  deriving (Generic, Show, Eq)

data EventSpec =
  EventSpec {
    esInputSpec  :: [InputSpec]
  , esOutputSpec :: [OutputSpec]
  }
  deriving (Generic, Show, Eq)

newtype Config =
  Config {
    eventEntries :: HashMap EventName EventSpec
  }
  deriving (Generic, Show, Eq)

--------------------------------------------------------------------------------

parseWorkerSpec :: JSON.Value -> JSON.Parser WorkerSpec
parseWorkerSpec value =
  case value of
    JSON.Object workerObj ->
      WorkerSpec
      <$> workerObj .: "timeout_ms"
      <*> workerObj .: "count"
    _ ->
      JSON.typeMismatch "Paseo.WorkerSpec" value


parseMemoryQueueInput :: JSON.Value -> JSON.Parser InputSpec
parseMemoryQueueInput value =
  case value of
    JSON.Object memoryQueueObj -> do
      MemoryQueueSpec
        <$> memoryQueueObj .: "max_size"
        <*> (memoryQueueObj .: "worker"
             >>= parseWorkerSpec)
        <*> memoryQueueObj .: "retry_after_ms"
    _ ->
      JSON.typeMismatch "Paseo.InputSpec" value

instance JSON.FromJSON InputSpec where
  parseJSON =
    parseMemoryQueueInput

-- -- TODO: OutputSpec parser

-- instance JSON.FromJSON OutputSpec where
--   parseJSON _ =
--     return OutputSpec

-- parseEventSpec :: JSON.Value -> JSON.Parser EventSpec
-- parseEventSpec value =
--   case value of
--     JSON.Object eventObj ->
--       EventSpec
--       <$> eventObj .: "input"
--       <*> (fromMaybe [] <$> eventObj .: "output")
--     _ ->
--       JSON.typeMismatch "Paseo.EventSpec" value

-- instance JSON.FromJSON EventSpec where
--   parseJSON =
--     parseEventSpec
