{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Driven.Internal.IO.Output where

import Protolude

import Data.HashMap.Strict (HashMap)

import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified JSONSchema.Draft4 as D4

import Control.Driven.Internal.Types

emitOutputEvent
  :: (DrivenEvent -> IO ())
  -> HashMap EventName (EventSpec, [Output])
  -> SomeOutputEvent
  -> IO ()
emitOutputEvent emitDrivenEvent outputMap someEvent =
  case HashMap.lookup (eventName someEvent) outputMap of
    Nothing ->
      -- This should _never_ happen
      emitDrivenEvent (EventOutputMissconfigured $ eventName someEvent)
    Just (eventSpec, outputs)  ->
      case (someEvent, esSchema eventSpec) of
        (JsonOutputEvent event, JsonSchema jsonSchemaPath) ->
          let
            eventJson =
              JSON.toJSON event

          in do
            schemaJson <-
              (fromMaybe (panic "Invalid JSON format on schema") . JSON.decodeStrict)
              <$> BS.readFile (Text.unpack jsonSchemaPath)
            result <-
              D4.fetchHTTPAndValidate (D4.SchemaWithURI schemaJson Nothing)
                                      eventJson

            case result of
              Left _ ->
                -- TODO: Change this to a logger
                putStrLn ("JSON serialization doesn't comply with JSON schema" :: Text)
              Right _ ->
                mapM_ (`writeToOutput` LBS.toStrict (JSON.encode eventJson))
                      outputs

        (ProtoOutputEvent event, Protobuffer) ->
          let
            serializedOutput =
              toProtobuff event
          in
            mapM_ (`writeToOutput` serializedOutput) outputs

        (codeConstraint, configConstraint) ->
          let
            codeConstraintName =
              case codeConstraint of
                ProtoOutputEvent {} ->
                  "protobuffer"
                JsonOutputEvent {} ->
                  "json_schema"
            configConstraintName =
              case configConstraint of
                Protobuffer ->
                  "protobuffer"
                JsonSchema {} ->
                  "json_schema"
          in
            emitDrivenEvent
              $ EventSerializerMissconfigured
                  (eventName someEvent) codeConstraintName configConstraintName
