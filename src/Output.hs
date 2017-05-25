{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Output where

import Protolude

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Config

--------------------------------------------------------------------------------

createMemoryQueueOutput
  :: EventName
  -> HashMap InputName Input
  -> InputName
  -> IO Output
createMemoryQueueOutput eventName allInputs inputName =
  case HashMap.lookup inputName allInputs of
    Nothing ->
      throwIO $ InputNameNotFound eventName inputName
    Just eventInput ->
      return $ MemoryOutput (writeToInput eventInput)

createOutput
  :: EventName
  -> HashMap InputName Input
  -> OutputSpec
  -> IO Output
createOutput eventName allInputs outputSpec =
  case outputSpec of
    OutputMemoryQueueSpec {..} ->
      createMemoryQueueOutput eventName allInputs osName

collectOutputsPerEventName
  :: HashMap EventName EventSpec
  -> HashMap OutputName Output
  -> IO (HashMap EventName (EventSpec, [Output]))
collectOutputsPerEventName allEvents allOutputs =
  let
    step acc (eventName, eventSpec) = do
      outputs <-
        forM (esOutputNames eventSpec) $ \outputName -> do
          maybe (throwIO $ OutputNameNotFound eventName outputName)
                return
                (HashMap.lookup outputName allOutputs)
      return $ HashMap.insert eventName (eventSpec, outputs) acc

  in
    foldM step HashMap.empty (HashMap.toList allEvents)
