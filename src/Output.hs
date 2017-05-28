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
  :: HashMap InputName Input
  -> InputName
  -> IO Output
createMemoryQueueOutput allInputs inputName =
  case HashMap.lookup inputName allInputs of
    Nothing ->
      throwIO $ InputNameNotFound inputName
    Just eventInput ->
      return $ Output (writeToInput eventInput)

createOutput
  :: HashMap InputName Input
  -> OutputSpec
  -> IO Output
createOutput allInputs outputSpec =
  case outputSpec of
    OutputMemoryQueueSpec {..} ->
      createMemoryQueueOutput allInputs osName
