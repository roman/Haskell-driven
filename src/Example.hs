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

data MessageQueued
  = MessageQueued { mqMessageId :: Text }
  deriving (Generic, JSON.FromJSON)

data TopicValidated
  = TopicValidated { tvMessageId :: Text }
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)


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

instance IEvent TopicValidated where
  eventName _ = "topic_validated"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  Right drivenConfig <- YAML.decodeFileEither "./resources/config/spec.yaml"
  drivenRuntime <-
    startSystem drivenConfig [("message_queued", [jsonHandler handleMessageReceived])]
  case HashMap.lookup "mem_message_queued" (runtimeInputs drivenRuntime) of
    Nothing ->
      putStrLn ("wtf" :: Text)
    Just input -> do
      putStrLn ("fuubar" :: Text)
      writeToInput input "{\"mrMessageId\": \"abc-123\"}"
