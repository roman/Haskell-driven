{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude
import System.IO (hSetBuffering, BufferMode(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.TH as JSON
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (camelTo2, fieldLabelModifier)
import qualified Data.Yaml as YAML
import Data.FileEmbed (embedFile)

-- import Proto.MessageQueuedV10 (MessageQueued(..))

import Control.Driven.Internal.Types
import Control.Driven.Internal.Core

import Control.Driven.Schema.JsonSchema as J

--------------------------------------------------------------------------------

data MessageQueued
  = MessageQueued { mqMessageId :: Text, mqPayload :: JSON.Value }
  deriving (Generic)

$(JSON.deriveJSON
   (JSON.defaultOptions {
      JSON.fieldLabelModifier = JSON.camelTo2 '_' . drop 2
    })
  ''MessageQueued)

instance IOutputEvent MessageQueued where
  eventName _ = "message_queued"

handleMessageQueued :: MessageQueued -> IO [SomeOutputEvent]
handleMessageQueued (MessageQueued msgId _payload) = do
  putStrLn $ "===> message_queued " <> msgId
  return [J.json $ TopicValidated "abc-123"]

--------------------

data TopicValidated
  = TopicValidated { tvMessageId :: Text }
  deriving (Generic)

$(JSON.deriveJSON
   (JSON.defaultOptions {
      JSON.fieldLabelModifier = JSON.camelTo2 '_' . drop 2
    })
  ''TopicValidated)


instance IOutputEvent TopicValidated where
  eventName _ = "topic_validated"

handleTopicValidated :: TopicValidated -> IO [SomeOutputEvent]
handleTopicValidated (TopicValidated msgId) = do
  putStrLn $ "===> topic_validated " <> msgId
  return []

--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let
    fileResult =
      YAML.decodeEither $(embedFile "./examples/driven-example/resources/config/spec.yaml")

  case fileResult of
    Left err ->
      print err
    Right drivenConfig -> do
      drivenRuntime <-
        startSystem
          drivenConfig
          print -- (putStrLn . JSON.encode)
          []
          [J.jsonSchema]
          [ ("message_queued",  [J.jsonHandler handleMessageQueued])
          , ("topic_validated", [J.jsonHandler handleTopicValidated]) ]
      case HashMap.lookup "mem_message_queued" (runtimeInputs drivenRuntime) of
        Nothing ->
          putStrLn ("runtimeInputs doesn't have defined input" :: Text)
        Just input -> do
          writeToInput input (LBS.toStrict $ JSON.encode $ MessageQueued "hello" $ JSON.object [])

      threadDelay 5000000
      stopSystem drivenRuntime
