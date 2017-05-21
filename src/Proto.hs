-- | Example of a library file. It is also used for testing the test suites.
{-# LANGUAGE ScopedTypeVariables #-}
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
module Proto where

import Protolude
import GHC.TypeLits (Symbol, KnownSymbol, SomeSymbol(..), sameSymbol, someSymbolVal)
import Data.HashMap.Strict (HashMap)

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ProtoLens as Proto (Message)
import qualified Data.ProtoLens.Encoding as Proto
import Proto.MessageQueuedV10 (MessageQueued(..))

--------------------------------------------------------------------------------

data SomeEventHandler
  = forall evId. (KnownSymbol evId, IEventHandler evId)
    => SomeEventHandler (Proxy evId)

data SomeOutputEvent
  = forall ev. (IOutputEvent ev)
    => SomeOutputEvent ev

class Proto.Message ev => IOutputEvent ev where
  _eventName :: ev -> Text
  _encode :: ev -> BS.ByteString
  _encode = Proto.encodeMessage

class Proto.Message (Event evId) => IEventHandler (evId :: Symbol) where
  type Event evId :: *
  _handleEvent :: Proxy evId -> Event evId -> IO [SomeOutputEvent]

instance IEventHandler "message_queued" where
  type Event "message_queued" = MessageQueued
  _handleEvent _ messageQueued = do
    print messageQueued
    return []

allHandlers :: HashMap Text SomeEventHandler
allHandlers =
  HashMap.fromList [
    ("message_queued"
    , SomeEventHandler (Proxy :: Proxy "message_queued"))
  ]

handleEvents :: HashMap Text [SomeEventHandler] -> Text -> BS.ByteString -> IO [SomeOutputEvent]
handleEvents handlerMap key input =
  let
    handleEvent :: KnownSymbol s => Proxy s -> SomeEventHandler -> IO [SomeOutputEvent]
    handleEvent inputProxy (SomeEventHandler handlerProxy) =
      if isJust (sameSymbol inputProxy handlerProxy) then
        case Proto.decodeMessage input of
          Left err -> do
            putStrLn err
            return []
          Right event ->
            _handleEvent handlerProxy event
      else
        return []

  in
    case someSymbolVal (Text.unpack key) of
      SomeSymbol inputProxy ->
        case HashMap.lookup key handlerMap of
          Nothing ->
            return []
          Just handlers ->
            concatMapM (handleEvent inputProxy) handlers


--------------------------------------------------------------------------------
