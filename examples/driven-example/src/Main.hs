{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import Protolude
import System.IO (BufferMode (..), hSetBuffering)

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.TH        as JSON
import qualified Data.Aeson.Types     as JSON (camelTo2, fieldLabelModifier)
import qualified Data.ByteString.Lazy as LBS
import           Data.FileEmbed       (embedFile)
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Yaml            as YAML

-- import Proto.MessageQueuedV10 (MessageQueued(..))

import Control.Driven.Internal.Core
import Control.Driven.Internal.Types

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

newtype TopicValidated
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
        Just input ->
          writeToInput input (LBS.toStrict $ JSON.encode $ MessageQueued "hello" $ JSON.object [])

      threadDelay 5000000
      stopSystem drivenRuntime
