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
module Backend where

import Protolude
import qualified Proto
import qualified JSON
import Data.HashMap.Strict (HashMap)


data SchemaBackend eventHandlerTy outEventTy =
  SchemaBackend {
      eventCallbacks :: HashMap Text [eventHandlerTy]
    , handleEvent :: HashMap Text [eventHandlerTy] -> Text -> ByteString -> IO [outEventTy]
    }

jsonSchema
  :: HashMap Text [JSON.SomeEventHandler]
  -> SchemaBackend JSON.SomeEventHandler JSON.SomeOutputEvent
jsonSchema callbacks =
  SchemaBackend {
      eventCallbacks = callbacks
    , handleEvent = JSON.handleEvents
    }

protoBuffer
  :: HashMap Text [Proto.SomeEventHandler]
  -> SchemaBackend Proto.SomeEventHandler Proto.SomeOutputEvent
protoBuffer callbacks =
  SchemaBackend {
      eventCallbacks = callbacks
    , handleEvent = Proto.handleEvents
    }

{-
initEventSystem config [("message_queued", onMessageQueued)]

-}
