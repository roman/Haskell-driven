{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Driven.Transport.Aws.Sqs where

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

readSqsMessage :: AWS.Env -> Text -> Int -> IO [(ByteString, IO ())]
readSqsMessage awsEnv queueUrl numberOfMessages =
    AWS.runResourceT $ AWS.runAWST awsEnv $ do
      messageList <- ensureRead
      forM messageList $ \msg ->
        return ( maybe mempty Text.encodeUtf8 (msg ^. SQS.mBody)
               , deleteSqsMessage awsEnv msg )
  where
    request =
      SQS.receiveMessage queueUrl
      & SQS.rmWaitTimeSeconds .~ Just 20
      & SQS.rmMaxNumberOfMessages .~ Just numberOfMessages

    handleResponse response =
      if response ^. SQS.rmrsResponseStatus /= 200 then
        -- TODO: log response status code
        ensureRead
      else case response ^. SQS.rmrsMessages of
        [] ->
          ensureRead
        messageList ->
          return messageList

    ensureRead =
      AWS.send request >>= handleResponse

writeSqsMessage :: AWS.Env -> Text -> ByteString -> IO ()
writeSqsMessage awsEnv queueUrl outputBytes =
    AWS.runResourceT $ AWS.runAWST awsEnv ensureWrite
  where
    request =
      SQS.sendMessage queueUrl (Text.decodeUtf8 outputBytes)

    handleResponse response =
      when (response ^. SQS.smrsResponseStatus /= 200)
        -- TODO: Naive approach, retry again immediately
        -- Need to work on a reliable dispatcher API
        ensureWrite

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
  if isTransportName == "sqs" then
    case JSON.parseEither (.: "queue_url") isObject of
      Left err ->
        throwIO $ InputCreationError spec (Text.pack err)
      Right queueUrl ->
        return (Just queueUrl)
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
