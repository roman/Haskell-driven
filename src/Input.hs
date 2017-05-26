{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Input where

import Protolude

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (newTBQueueIO, writeTBQueue, readTBQueue, unGetTBQueue)
import Config

--------------------------------------------------------------------------------

createMemoryQueueInput :: Int -> Int -> IO Input
createMemoryQueueInput retryMillis totalSize = do
  queue <- newTBQueueIO totalSize
  let
    unread =
      atomically . unGetTBQueue queue

    read = do
      message <- atomically $ readTBQueue queue
      retryAsync <- async $ threadDelay retryMillis -- >> unread message
      return (message, cancel retryAsync)

    write =
      atomically . writeTBQueue queue

  return $ Input read write


createInput :: InputSpec -> IO Input
createInput inputSpec =
  case inputSpec of
    InputMemoryQueueSpec {..} ->
      createMemoryQueueInput isRetryAfterMs isMaxSize
