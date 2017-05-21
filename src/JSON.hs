-- | Example of a library file. It is also used for testing the test suites.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Protolude
import GHC.TypeLits (Symbol, KnownSymbol, SomeSymbol(..), sameSymbol, someSymbolVal)
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified JSONSchema.Draft4 as D4

--------------------------------------------------------------------------------

data MessageQueued
  = MessageQueued {
      messageId :: Text
    , payload   :: Value
    }
  deriving (Generic, FromJSON)

data TopicValidated
  = TopicValidated
  deriving (Generic)

instance FromJSON TopicValidated where
  parseJSON value =
    case value of
      Object obj -> do
        ty <- obj .: "type"
        if ty == ("topic_validated" :: Text) then
          return TopicValidated
        else
          typeMismatch "TopicValidated" value
      _ ->
        typeMismatch "TopicValidated" value


class ToJSON ev => IOutputEvent ev where
  _eventName :: ev -> Text
  _encode :: ev -> BS.ByteString
  _encode = LBS.toStrict . encode

class FromJSON (Event evId) => IEventHandler (evId :: Symbol) where
  type Event evId :: *
  _handleEvent :: Proxy evId -> Event evId -> IO [SomeOutputEvent]

data SomeOutputEvent
  = forall ev. (IOutputEvent ev)
    => SomeOutputEvent ev

data SomeEventHandler
  = forall evId. (KnownSymbol evId, IEventHandler evId) => SomeEventHandler (Proxy evId)

instance IEventHandler "message_queued" where
  type Event "message_queued" = MessageQueued
  _handleEvent _ (MessageQueued {}) = do
    putStrLn ("MessageQueued" :: Text)
    return []

instance IEventHandler "topic_validated" where
  type Event "topic_validated" = TopicValidated
  _handleEvent _ TopicValidated = do
    putStrLn ("TopicValidated" :: Text)
    return []

type EventHandlers = HashMap Text SomeEventHandler

allHandlers :: EventHandlers
allHandlers =
  HashMap.fromList [
    ("message_queued"
    , SomeEventHandler (Proxy :: Proxy "message_queued"))

  , ("topic_validated"
    , SomeEventHandler (Proxy :: Proxy "topic_validated"))
  ]

handleEvent :: EventHandlers -> Text -> BS.ByteString -> IO [SomeOutputEvent]
handleEvent handlers key payload' =
  case someSymbolVal (Text.unpack key) of
    SomeSymbol inputProxy ->
      case HashMap.lookup key handlers of
        Nothing ->
          return []
        Just (SomeEventHandler handlerProxy) ->
          if (isJust (sameSymbol inputProxy handlerProxy)) then do
            case decodeStrict payload' of
              Nothing -> do
                putStrLn ("ignore" :: Text)
                return []
              Just event ->
                _handleEvent handlerProxy event
          else
            return []

--------------------------------------------------------------------------------

parseMessageQueued :: ByteString -> IO (Either D4.HTTPValidationFailure ())
parseMessageQueued input0 = do
  bts <- BS.readFile "resources/private/schema/json/message_queued_v1.0.json"
  let
    input =
      fromMaybe (panic "invalid json") (decodeStrict input0)

    schema =
      fromMaybe (panic "invalid json schema") (decodeStrict bts)

    schemaWithURI =
      D4.SchemaWithURI schema Nothing

  D4.fetchHTTPAndValidate schemaWithURI input
