{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module IO.Memory where

import Protolude

import Data.HashMap.Strict (HashMap)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue, readTBQueue, unGetTBQueue)

import Data.Aeson as JSON ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, parseEither)
import qualified Data.HashMap.Strict as HashMap

import qualified Data.Text as Text

import Config

--------------------------------------------------------------------------------
-- Input

parseMemoryQueue
  :: HashMap Text JSON.Value
  -> JSON.Parser (Int, Int)
parseMemoryQueue memoryQueueObj =
  (,)
    <$> memoryQueueObj .: "max_size"
    <*> memoryQueueObj .: "retry_after_ms"

createMemoryQueueInput :: Int -> Int -> IO Input
createMemoryQueueInput retryMillis totalSize = do
  queue <- newTBQueueIO totalSize
  let
    unread =
      atomically . unGetTBQueue queue

    read = do
      message <- atomically $ readTBQueue queue
      retryAsync <-
        async $ threadDelay (retryMillis * 1000) >> unread message
      return (message, cancel retryAsync)

    write =
      atomically . writeTBQueue queue

  return $ Input read write

memCreateInput :: InputSpec -> IO Input
memCreateInput spec@(InputSpec {..}) =
  case JSON.parseEither parseMemoryQueue isObject of
    Left err ->
      throwIO $ InputCreationError spec (Text.pack err)
    Right (retryMillis, totalSize) ->
      createMemoryQueueInput retryMillis totalSize

--------------------------------------------------------------------------------
-- Output

createMemoryQueueOutput
  :: HashMap InputName Input
  -> InputName
  -> IO Output
createMemoryQueueOutput allInputs inputName =
  case HashMap.lookup inputName allInputs of
    Nothing ->
      throwIO $ InputNameNotFound inputName
    Just eventInput ->
      return $ Output (writeToInput eventInput)

memCreateOutput
  :: HashMap InputName Input
  -> OutputSpec
  -> IO Output
memCreateOutput allInputs (OutputSpec {..}) =
  createMemoryQueueOutput allInputs osName

--------------------------------------------------------------------------------

memoryBackend :: Backend
memoryBackend =
  Backend { createInput  = memCreateInput
          , createOutput = memCreateOutput }
