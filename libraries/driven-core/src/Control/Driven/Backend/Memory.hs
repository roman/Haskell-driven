{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Control.Driven.Backend.Memory where

import Protolude

import Data.HashMap.Strict (HashMap)

import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TBQueue
    (newTBQueueIO, readTBQueue, unGetTBQueue, writeTBQueue)

import           Data.Aeson          as JSON ((.:))
import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON (Parser, parseEither)
import qualified Data.HashMap.Strict as HashMap

import qualified Data.Text as Text

import Control.Driven.Internal.Types

--------------------------------------------------------------------------------
-- Input

parseMemoryQueue
  :: HashMap Text JSON.Value
  -> JSON.Parser (Int, Int)
parseMemoryQueue memoryQueueObj =
  (,)
    <$> memoryQueueObj .: "max_size"
    <*> memoryQueueObj .: "retry_after_ms"

createMemoryQueueInput
  :: (DrivenEvent -> IO ())
  -> InputName
  -> TimeoutMillis
  -> Size
  -> IO Input
createMemoryQueueInput emitEvent inputName retryMillis totalSize = do
  queue <- newTBQueueIO totalSize
  let
    unread =
      atomically . unGetTBQueue queue

    read = do
      message <- atomically $ readTBQueue queue
      -- TODO: Make this more efficient by having a single thread that deals
      -- with re-send every millisecond
      retryAsync <-
        async $ do
          threadDelay (retryMillis * 1000)
          unread message
      return (message, cancel retryAsync)

    write =
      atomically . writeTBQueue queue

  emitEvent $ InputCreated "memory_queue" inputName
  return
    $ Input read write (emitEvent $ InputDisposed "memory_queue" inputName)

memCreateInput
  :: (DrivenEvent -> IO ())
  -> InputSpec
  -> IO (Maybe Input)
memCreateInput emitEvent spec@InputSpec {..} =
  if isBackendName == "memory_queue" then
    case JSON.parseEither parseMemoryQueue isObject of
      Left err ->
        throwIO $ InputCreationError spec (Text.pack err)
      Right (totalSize, retryMillis) ->
        Just <$> createMemoryQueueInput emitEvent isName retryMillis totalSize
  else
    return Nothing

--------------------------------------------------------------------------------
-- Output

memCreateOutput
  :: (DrivenEvent -> IO ())
  -> HashMap InputName Input
  -> OutputSpec
  -> IO (Maybe Output)
memCreateOutput emitEvent allInputs OutputSpec {..} =
  if osBackendName == "memory_queue" then
    case HashMap.lookup osName allInputs of
      Nothing ->
        throwIO $ InputNameNotFound osName
      Just eventInput -> do
        emitEvent $ OutputCreated "memory_queue" osName
        return
          $ Just
          $ Output (writeToInput eventInput)
                   (emitEvent $ OutputDisposed "memory_queue" osName)
  else
    return Nothing

--------------------------------------------------------------------------------

memoryBackend :: Backend
memoryBackend =
  Backend { createInput  = memCreateInput
          , createOutput = memCreateOutput }
