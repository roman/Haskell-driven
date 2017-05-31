{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Driven.Internal.Schema.Protobuff where

import Protolude

import Data.Aeson ((.:))

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, parseEither, typeMismatch)

import Control.Driven.Internal.Types

--------------------------------------------------------------------------------

data ProtobuffEventHandler
  = forall event. FromProtobuff event
     => ProtobuffEventHandler (event -> IO [SomeOutputEvent])

instance IEventHandler ProtobuffEventHandler where
  handlerTypeName _ = "protobuff"
  handleEvent (ProtobuffEventHandler handler) inputBytes =
    case fromProtobuff inputBytes of
      Nothing ->
        return []
      Just inputEvent ->
        handler inputEvent

protobuffHandler
  :: FromProtobuff event
  => (event -> IO [SomeOutputEvent])
  -> SomeEventHandler
protobuffHandler =
  SomeEventHandler . ProtobuffEventHandler

--------------------

parseProtobuffSchemaSpec :: JSON.Value -> JSON.Parser ()
parseProtobuffSchemaSpec schemaValue = do
  case schemaValue of
    JSON.Object schemaObj -> do
      schemaTy <- schemaObj .: "type"
      if schemaTy == ("protobuff" :: Text) then
        return ()
      else
        JSON.typeMismatch "Driven.ProtobuffSchemaSpec" schemaValue

    _ ->
      JSON.typeMismatch "Driven.ProtobuffSchemaSpec" schemaValue

protobuffSchema :: SchemaSpec
protobuffSchema eventSpec =
  let
    schemaSpecValue =
      esSchema eventSpec
  in
    case JSON.parseEither parseProtobuffSchemaSpec schemaSpecValue of
      Left _ ->
        return Nothing

      Right () ->
        Just <$> return (const $ Right ())

--------------------

data ProtobuffOutputEvent
  = forall event. (IOutputEvent event, ToProtobuff event)
    => ProtobuffOutputEvent event

instance IOutputEvent ProtobuffOutputEvent where
  eventName (ProtobuffOutputEvent event) =
    eventName event

instance IOutputSerializer ProtobuffOutputEvent where
  serializeEvent (ProtobuffOutputEvent event) =
    toProtobuff event

protobuff
  :: (IOutputEvent event, ToProtobuff event)
  => event
  -> SomeOutputEvent
protobuff =
  SomeOutputEvent . ProtobuffOutputEvent

--------------------------------------------------------------------------------
