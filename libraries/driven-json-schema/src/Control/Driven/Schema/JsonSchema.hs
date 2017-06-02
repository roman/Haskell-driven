{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
module Control.Driven.Schema.JsonSchema where

import Protolude

import Data.Aeson ((.:))

import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON (Parser, parseEither, typeMismatch)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import qualified JSONSchema.Draft4    as D4

import Control.Driven.Types

--------------------------------------------------------------------------------

data JsonEventHandler
  = forall event. JSON.FromJSON event
    => JsonEventHandler (event -> IO [SomeOutputEvent])

instance IEventHandler JsonEventHandler where
  handlerTypeName _ = "json_schema"
  handleEvent (JsonEventHandler handler) inputBytes =
    case JSON.eitherDecodeStrict inputBytes of
      Left err ->
        return
          $ Left
          $ toException
          $ EventHandlerInputParserFailed (Text.pack err)
      Right inputEvent ->
        Right <$> handler inputEvent

jsonHandler
  :: JSON.FromJSON event
  => (event -> IO [SomeOutputEvent])
  -> SomeEventHandler
jsonHandler = SomeEventHandler . JsonEventHandler

--------------------

data JsonSchemaError
  = JsonSchemaFileFormatError Text
  | JsonSchemaHttpFailure D4.HTTPFailure
  | JsonSchemaInvalid D4.SchemaInvalid
  | JsonSchemaMissmatch [D4.ValidatorFailure]
  deriving (Show)

instance Exception JsonSchemaError

newtype JsonSchemaSpec
  = JsonSchemaSpec
    { jsonSchemaPath :: Text }

parseJsonSchemaSpec :: JSON.Value -> JSON.Parser JsonSchemaSpec
parseJsonSchemaSpec schemaValue =
  case schemaValue of
    JSON.Object schemaObj -> do
      schemaTy <- schemaObj .: "type"
      if schemaTy == ("json_schema" :: Text) then
        JsonSchemaSpec <$> schemaObj .: "path"
      else
        JSON.typeMismatch "Driven.JsonSchemaSpec" schemaValue
    _ ->
      JSON.typeMismatch "Driven.JsonSchemaSpec" schemaValue

_getJsonSchemaRecord
  :: JSON.FromJSON schema
  => Text
  -> IO (D4.SchemaWithURI schema)
_getJsonSchemaRecord schemaFile = do
  mSchemaValue <- JSON.decode <$> LBS.readFile (Text.unpack schemaFile)
  case mSchemaValue of
    Nothing ->
      throwIO $ JsonSchemaFileFormatError schemaFile
    Just schemaValue ->
      return $ D4.SchemaWithURI schemaValue Nothing

_fetchSchemaReferences
  :: D4.SchemaWithURI D4.Schema
  -> IO (D4.URISchemaMap D4.Schema)
_fetchSchemaReferences schemaSpec = do
  schemaResult <- D4.referencesViaHTTP schemaSpec
  case schemaResult of
    Left err ->
      throwIO $ JsonSchemaHttpFailure err
    Right schema ->
      return schema

_getSchemaValidator
  :: MonadIO m
  => D4.SchemaWithURI D4.Schema
  -> D4.URISchemaMap D4.Schema
  -> m Schema
_getSchemaValidator schemaSpec schema =
  let
    validateBytes validator outputBytes =
      let
        Just outputValue =
          JSON.decodeStrict outputBytes

        validationResult =
          validator outputValue
      in
        if null validationResult then
          Right ()
        else
          Left
            $ toException
            $ JsonSchemaMissmatch validationResult

  in
    case D4.checkSchema schema schemaSpec  of
      Left err ->
        throwIO $ JsonSchemaInvalid err
      Right validator ->
        return (validateBytes validator)

buildJsonSchema :: Text -> IO Schema
buildJsonSchema schemaFile = do
  schemaSpec <- _getJsonSchemaRecord schemaFile
  schema     <- _fetchSchemaReferences schemaSpec
  _getSchemaValidator schemaSpec schema

jsonSchema :: SchemaSpec
jsonSchema eventSpec =
  let
    schemaSpecValue =
      esSchema eventSpec
  in
    case JSON.parseEither parseJsonSchemaSpec schemaSpecValue of
      Left _ ->
        return Nothing

      Right (JsonSchemaSpec schemaPath) ->
        Just <$> buildJsonSchema schemaPath

--------------------

data JsonOutputEvent
  = forall event. (IOutputEvent event, JSON.ToJSON event)
    => JsonOutputEvent event

instance IOutputEvent JsonOutputEvent where
  eventName (JsonOutputEvent event) =
    eventName event

instance IOutputSerializer JsonOutputEvent where
  serializeEvent (JsonOutputEvent event) =
    let
      jsonValue = JSON.toJSON event
    in
      LBS.toStrict (JSON.encode jsonValue)

json
  :: (IOutputEvent event, JSON.ToJSON event)
  => event
  -> SomeOutputEvent
json =
  SomeOutputEvent . JsonOutputEvent
