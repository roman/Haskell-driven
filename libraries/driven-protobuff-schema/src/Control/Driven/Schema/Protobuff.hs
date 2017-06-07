{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
module Control.Driven.Schema.Protobuff where

import Protolude

import Data.Aeson ((.:))

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON (Parser, parseEither, typeMismatch)
import qualified Data.ProtoLens   as Proto (Message, decodeMessage, encodeMessage)
import qualified Data.Text        as Text

import Control.Driven.Types

--------------------------------------------------------------------------------

class FromProtobuff msg where
  fromProtobuff :: ByteString -> Either Text msg

instance Proto.Message msg => FromProtobuff msg where
  fromProtobuff input =
    case Proto.decodeMessage input of
      Left err ->
        Left $ Text.pack err
      Right result ->
        Right result

class ToProtobuff msg where
  toProtobuff :: msg -> ByteString

instance Proto.Message msg => ToProtobuff msg where
  toProtobuff = Proto.encodeMessage

--------------------

data ProtobuffEventHandler
  = forall event. FromProtobuff event
     => ProtobuffEventHandler (event -> IO [SomeOutputEvent])

instance IEventHandler ProtobuffEventHandler where
  handlerTypeName _ = "protobuff"
  handleEvent (ProtobuffEventHandler handler) inputBytes =
    case fromProtobuff inputBytes of
      Left err ->
        return
          $ Left
          $ toException
          $ EventHandlerInputParserFailed err

      Right inputEvent ->
        Right <$> handler inputEvent

protobuffHandler
  :: FromProtobuff event
  => (event -> IO [SomeOutputEvent])
  -> SomeEventHandler
protobuffHandler =
  SomeEventHandler . ProtobuffEventHandler

--------------------

parseProtobuffSchemaSpec :: JSON.Value -> JSON.Parser ()
parseProtobuffSchemaSpec schemaValue =
  case schemaValue of
    JSON.Object schemaObj -> do
      schemaTy <- schemaObj .: "type"
      unless (schemaTy == ("protobuff" :: Text)) $
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
  = forall event. (IEvent event, ToProtobuff event)
    => ProtobuffOutputEvent event

instance IEvent ProtobuffOutputEvent where
  eventKey (ProtobuffOutputEvent event) =
    eventKey event

instance IOutputSerializer ProtobuffOutputEvent where
  serializeEvent (ProtobuffOutputEvent event) =
    toProtobuff event

protobuff
  :: (IEvent event, ToProtobuff event)
  => event
  -> SomeOutputEvent
protobuff =
  SomeOutputEvent . ProtobuffOutputEvent

--------------------------------------------------------------------------------
