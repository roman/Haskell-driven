{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Driven.Backend.Aws.Sqs where

import Protolude

import Data.Aeson ((.:))
import Lens.Micro ((^.), (.~))

import qualified Data.Aeson.Types as JSON (parseEither)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8, decodeUtf8)
import qualified Control.Monad.Trans.AWS as AWS
import qualified Network.AWS.SQS as SQS

import Control.Driven.Types

deleteSqsMessage :: AWS.Env -> SQS.Message -> IO ()
deleteSqsMessage awsEnv msg =
    AWS.runResourceT $ AWS.runAWST awsEnv $
      maybe (return ()) (void . AWS.send) mRequest
  where
    mRequest = do
      msgId <- msg ^. SQS.mMessageId
      msgReceiptHandle <- msg ^. SQS.mReceiptHandle
      return $ SQS.deleteMessage msgId msgReceiptHandle

readSqsMessage :: AWS.Env -> Text -> IO (ByteString, IO ())
readSqsMessage awsEnv queueUrl =
    AWS.runResourceT $ AWS.runAWST awsEnv $ do
      msg <- ensureRead
      return ( maybe mempty Text.encodeUtf8 (msg ^. SQS.mBody)
             , deleteSqsMessage awsEnv msg )
  where
    request =
      SQS.receiveMessage queueUrl
      & SQS.rmWaitTimeSeconds .~ Just 20
      & SQS.rmMaxNumberOfMessages .~ Just 1

    handleResponse response =
      if response ^. SQS.rmrsResponseStatus /= 200 then
        -- TODO: log response status code
        ensureRead
      else case response ^. SQS.rmrsMessages of
        [x] ->
          return x
        (x:_) ->
          -- TODO: log warning (should never happen)
          return x
        _ ->
          -- TODO: log warning
          ensureRead

    ensureRead =
      AWS.send request >>= handleResponse

writeSqsMessage :: AWS.Env -> Text -> ByteString -> IO ()
writeSqsMessage awsEnv queueUrl outputBytes =
    AWS.runResourceT $ AWS.runAWST awsEnv ensureWrite
  where
    request =
      SQS.sendMessage queueUrl (Text.decodeUtf8 outputBytes)

    handleResponse response =
      if response ^. SQS.smrsResponseStatus /= 200 then
        -- TODO: Naive approach, retry again immediately
        -- Need to work on a reliable dispatcher API
        ensureWrite
      else
        return ()

    ensureWrite =
      AWS.send request >>= handleResponse

buildSqsInput
  :: (DrivenEvent -> IO ())
  -> AWS.Env
  -> InputName
  -> Text
  -> IO Input
buildSqsInput emitDrivenEvent awsEnv inputName queueUrl = do
 emitDrivenEvent $ InputCreated "sqs" inputName
 return $
   Input (readSqsMessage awsEnv queueUrl)
         (writeSqsMessage awsEnv queueUrl)
         (emitDrivenEvent $ InputDisposed "sqs" inputName)

parseSqsInputSpec :: InputSpec -> IO (Maybe Text)
parseSqsInputSpec spec@InputSpec {..} =
  if isBackendName == "sqs" then
    case JSON.parseEither (.: "queue_url") isObject of
      Left err ->
        throwIO $ InputCreationError spec (Text.pack err)
      Right queueUrl -> do
        return $ Just queueUrl
  else
    return Nothing

sqsCreateInput
  :: AWS.Env
  -> (DrivenEvent -> IO ())
  -> InputSpec
  -> IO (Maybe Input)
sqsCreateInput awsEnv emitDrivenEvent spec = do
  mQueueUrl <- parseSqsInputSpec spec
  case mQueueUrl of
    Nothing ->
      return Nothing
    Just queueUrl ->
      Just <$> buildSqsInput emitDrivenEvent awsEnv (isName spec) queueUrl
