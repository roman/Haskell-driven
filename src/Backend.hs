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
import qualified Lib
import Data.HashMap.Strict (HashMap)


data SchemaBackend eventHandlerTy outEventTy =
  SchemaBackend {
      eventCallbacks :: HashMap Text eventHandlerTy
    , handleEvent :: HashMap Text eventHandlerTy -> Text -> ByteString -> IO [outEventTy]
    }

jsonSchema
  :: HashMap Text Lib.SomeEventHandler
  -> SchemaBackend Lib.SomeEventHandler Lib.SomeOutputEvent
jsonSchema callbacks =
  SchemaBackend {
      eventCallbacks = callbacks
    , handleEvent = Lib.handleEvent
    }

protoBuffer
  :: HashMap Text Proto.SomeEventHandler
  -> SchemaBackend Proto.SomeEventHandler Proto.SomeOutputEvent
protoBuffer callbacks =
  SchemaBackend {
      eventCallbacks = callbacks
    , handleEvent = Proto.handleEvent
    }

{-
initEventSystem config [("message_queued", onMessageQueued)]

-}
