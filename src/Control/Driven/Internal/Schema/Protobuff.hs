{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Driven.Internal.Schema.Protobuff where

import Protolude

import Data.Aeson ((.:))

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (Parser, parseEither, typeMismatch)
import qualified Data.Text as Text

import qualified Data.ProtoLens       as Proto (Message, decodeMessage, encodeMessage)
import Control.Driven.Internal.Types

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
