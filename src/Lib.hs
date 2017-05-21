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
module Lib where

import Protolude
import GHC.TypeLits (Symbol, KnownSymbol, SomeSymbol(..), sameSymbol, someSymbolVal)
import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict (HashMap)

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified JSONSchema.Draft4 as D4

--------------------------------------------------------------------------------

data MessageQueued
  = MessageQueued {
      messageId :: Text
    , payload   :: Value
    }
  deriving (Generic, FromJSON)

-- instance FromJSON MessageQueued where
--   parseJSON value =
--     case value of
--       Object obj -> do
--         ty <- obj .: "type"
--         if ty == ("message_queued" :: Text) then
--           return MessageQueued
--         else
--           typeMismatch "MessageQueued" value
--       _ ->
--         typeMismatch "MessageQueued" value

-- instance ToJSON MessageQueued where
--   toJSON _ =
--     object [("type" .= ("message_queued" :: Text))]

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


class FromJSON (Event evId) => IEventHandler (evId :: Symbol) where
  type Event evId :: *
  _handleEvent :: Proxy evId -> Event evId -> IO ()

data SomeEventHandler
  = forall evId. (KnownSymbol evId, IEventHandler evId) => SomeEventHandler (Proxy evId)

instance IEventHandler "message_queued" where
  type Event "message_queued" = MessageQueued
  _handleEvent _ (MessageQueued {}) =
    putStrLn ("MessageQueued" :: Text)

instance IEventHandler "topic_validated" where
  type Event "topic_validated" = TopicValidated
  _handleEvent _ TopicValidated =
    putStrLn ("TopicValidated" :: Text)

type EventHandlers = HashMap Text SomeEventHandler

allHandlers :: EventHandlers
allHandlers =
  HashMap.fromList [
    ("message_queued"
    , SomeEventHandler (Proxy :: Proxy "message_queued"))

  , ("topic_validated"
    , SomeEventHandler (Proxy :: Proxy "topic_validated"))
  ]

handleEvent :: EventHandlers -> Text -> LB8.ByteString -> IO ()
handleEvent handlers key payload =
  case someSymbolVal (Text.unpack key) of
    SomeSymbol inputProxy ->
      case HashMap.lookup key handlers of
        Nothing ->
          return ()
        Just (SomeEventHandler handlerProxy) ->
          when (isJust (sameSymbol inputProxy handlerProxy)) $ do
            case decode payload of
              Nothing ->
                putStrLn ("ignore" :: Text)
              Just event ->
                _handleEvent handlerProxy event

--------------------------------------------------------------------------------
