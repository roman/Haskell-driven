{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Bootstraph where

import Protolude
import Config (Config)
import Data.HashMap.Strict (HashMap)

import qualified Config
import qualified Data.HashMap.Strict as HashMap

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueueIO, TBQueue, readTBQueue)

import qualified JSON
import qualified Proto

data EventLoop
  = EventLoop {
    _eventLoopQueue :: TBQueue ByteString
  , _eventLoopAsync :: Async ()
  }

workerSpecToEventLoop
  :: Config.EventCallbackRegistry "json_schema"
  -> Config.WorkerSpec
  -> IO (Async ())
workerSpecToEventLoop (Config.ECR callbacks) (Config.WorkerSpec _ evName evSchema) = do
  inputQueue <- newTBQueueIO 1000
  let
    handleEvent =
      case evSchema of
        Config.JSONSchema _schemaFile ->
          JSON.handleEvents callbacks

  async $ forever $ do
    input <- atomically $ readTBQueue inputQueue
    result  <- try $ handleEvent evName input
    case result of
      Left (err :: SomeException) ->
        -- TODO: Logging Strategy, Error Reporter call
        print err
      Right _outEvents ->
        putStrLn ("PENDING: Publishing outEvents" :: Text)
