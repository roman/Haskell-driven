{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Example where

import Protolude

import System.IO (hSetBuffering, BufferMode(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson as JSON
import qualified Data.Yaml as YAML

import Proto.MessageQueuedV10 (MessageQueued(..))

import Config
import Core

--------------------------------------------------------------------------------

instance IEvent MessageQueued where
  eventName = const "message_queued"

handleMessageReceived :: Msg MessageQueued -> IO [SomeOutputEvent]
handleMessageReceived (Msg deleteMsg m@(MessageQueued {})) = do
  print m
  deleteMsg
  return [json $ TopicValidated "abc-123"]

--------------------------------------------------------------------------------

-- data MessageReceived
--   = MessageReceived { mrMessageId :: Text }
--   deriving (Generic, JSON.FromJSON, JSON.ToJSON)

-- instance IEvent MessageReceived where
--   eventName _ = "message_queued"

-- handleMessageReceived :: Msg MessageReceived -> IO [SomeOutputEvent]
-- handleMessageReceived (Msg deleteMsg (MessageReceived msgId)) = do
--   putStrLn $ "message_queued " <> msgId
--   deleteMsg
--   return [json $ TopicValidated "abc-123"]

--------------------

data TopicValidated
  = TopicValidated { tvMessageId :: Text }
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)

instance IEvent TopicValidated where
  eventName _ = "topic_validated"

handleTopicValidated :: Msg TopicValidated -> IO [SomeOutputEvent]
handleTopicValidated (Msg deleteMsg (TopicValidated msgId)) = do
  putStrLn $ "topic_validated " <> msgId
  deleteMsg
  return []

--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  fileResult <- YAML.decodeFileEither "./resources/config/spec.yaml"
  case fileResult of
    Left err ->
      print err
    Right drivenConfig -> do
      drivenRuntime <-
        startSystem
          drivenConfig
          (putStrLn . JSON.encode)
          []
          [ ("message_queued",  [protoHandler handleMessageReceived])
          , ("topic_validated", [jsonHandler handleTopicValidated]) ]
      case HashMap.lookup "mem_message_queued" (runtimeInputs drivenRuntime) of
        Nothing ->
          putStrLn ("runtimeInputs doesn't have defined input" :: Text)
        Just input -> do
          -- writeToInput input "{\"mrMessageId\": \"abc-123\"}"
          writeToInput input (toProtobuff $ MessageQueued "hello" [])

      threadDelay 5000000
      stopSystem drivenRuntime
