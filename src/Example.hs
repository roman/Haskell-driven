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

import Config
import Core

--------------------------------------------------------------------------------

data MessageQueued
  = MessageQueued { mqMessageId :: Text }
  deriving (Generic, JSON.FromJSON)


data MessageReceived
  = MessageReceived { mrMessageId :: Text }
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)

instance IEvent MessageReceived where
  eventName _ = "message_queued"

handleMessageReceived :: Msg MessageReceived -> IO [SomeOutputEvent]
handleMessageReceived (Msg deleteMsg (MessageReceived msgId)) = do
  putStrLn $ "message_queued " <> msgId
  deleteMsg
  return [json $ TopicValidated "abc-123"]

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
          [ ("message_queued", [jsonHandler handleMessageReceived])
          , ("topic_validated", [jsonHandler handleTopicValidated]) ]
      case HashMap.lookup "mem_message_queued" (runtimeInputs drivenRuntime) of
        Nothing ->
          putStrLn ("runtimeInputs doesn't have defined input" :: Text)
        Just input -> do
          writeToInput input "{\"mrMessageId\": \"abc-123\"}"

      threadDelay 5000000
      stopSystem drivenRuntime
