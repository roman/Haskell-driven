{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Driven.Internal.Schema where

import Protolude

import qualified Data.HashMap.Strict as HashMap
import qualified JSONSchema.Draft4   as D4

import Control.Driven.Internal.Types

resolveJsonSchema schemaPath = do
   D4.referencesViaHttp

createSchemaMap
  :: (DrivenEvent -> IO ())
  -> DrivenConfig
  -> HashMap EventName EventSpec
  -> IO (HashMap EventName Schema)
createSchemaMap emitEvent drivenConfig eventSpecMap = do
  let
    step acc (evName, evSpec) =
      case esSchema evSpec of
        JsonSchemaSpec schemaPath -> do
          validator <- resolveJsonSchema schemaPath
          return $ JsonSpec validator
        ProtobufferSpec ->
          return Protobuffer
  in
    foldM step HashMap.empty $ HashMap.toList eventSpecMap
