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

import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON

import Config

data MessageQueued
  = MessageQueued { mqMessageId :: Text }
  deriving (Generic, JSON.FromJSON)

data TopicValidated
  = TopicValidated { tvMessageId :: Text }
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)

instance IEvent TopicValidated where
  eventName _ = "topic_validated"

eventSchemas =
  HashMap.fromList [
  ( "topic_validated"
  , ( EventSpec (JSONSchema "foobar") [] []
    , [Output print]))
  ]

main = do
  _emitEvent eventSchemas SJSON (SomeOutputEvent (TopicValidated "abc-123")) >>= print

-- instance IOutputEvent 'JSON TopicValidated where
--   _eventName _ _ = "topic_validated"
--   _encode _ = LBS.toStrict . JSON.encode

-- instance IEventHandler 'JSON "topic_validated" where
--   type Event "topic_validated" = TopicValidated
--   _handleEvent _ _ (TopicValidated {}) =
--     return []

-- instance IEventHandler 'JSON "message_queued" where
--   type Event "message_queued" = MessageQueued
--   _handleEvent _ _ (MessageQueued {}) =
--     return [json $ TopicValidated "abc-123"]
