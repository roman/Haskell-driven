{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Exp where

import Protolude
import GHC.Generics (Generic)
import Data.Aeson ((.:))


import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, parseEither, typeMismatch)
import qualified Data.ByteString.Lazy as LBS
import qualified JSONSchema.Draft4 as D4
import qualified Data.Text as Text

import Control.Driven.Internal.Types hiding
  (SomeOutputEvent, IEventHandler(..), SomeEventHandler, json, JsonOutputEvent, eventName, Schema)

--------------------------------------------------------------------------------


buildSchemaEntry
  :: SchemaSpec
  -> EventName
  -> EventSpec
  -> IO (Maybe (EventName, Schema))
buildSchemaEntry buildSchema evName eventSpec =
  ((evName,) <$>) <$> buildSchema eventSpec

--------------------------------------------------------------------------------

data Foobar
  = Foobar
    {
      foobarMsgId :: Text
    }
  deriving (Generic, Show, Eq, JSON.FromJSON)

data Evento
  = Evento
    {
      eventoMsgId :: Text
    }
  deriving (Generic, Show, Eq, JSON.ToJSON)

instance IOutputEvent Evento where
  eventName _ = "evento"

--------------------

h1 :: SomeEventHandler
h1 = jsonHandler $ \(Foobar msgId) -> do
  putStrLn $ "Message Id: " <> msgId
  return [json $ Evento msgId]

runEventHandler :: SomeEventHandler -> ByteString -> IO [ByteString]
runEventHandler (SomeEventHandler handler) input = do
  outputs <- handleEvent handler input
  return $ map serializeEvent outputs
