{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Driven.Transport.Aws.SqsTest where

import Protolude hiding ((&))

import Lens.Micro
import Test.Tasty.Hspec
import Test.Tasty.HUnit

import Control.Driven.Types
import qualified Control.Monad.Trans.AWS as AWS
import qualified Network.AWS.SQS as SQS
import qualified Control.Driven.Transport.Aws.Sqs as SUT

awsKeyId :: AWS.AccessKey
awsKeyId = "AKI00000000000000000"

awsSecretAccessKey :: AWS.SecretKey
awsSecretAccessKey="0000000000000000000000000000000000000000"

newEnv :: IO AWS.Env
newEnv = do
  logger <- AWS.newLogger AWS.Info stdout
  env0    <- AWS.newEnv (AWS.FromKeys awsKeyId awsSecretAccessKey)
  let
    env =
      env0
        & AWS.envLogger .~ logger
        & AWS.configure (AWS.setEndpoint False "localhost" 47195 SQS.sqs)
  return env

spec_sqsMessageRoundtrip :: Spec
spec_sqsMessageRoundtrip =
  describe "when writing and reading message from queue" $
    it "returns same contents" $ do
      awsEnv <- newEnv
      let
        inputBytes = "{\"Hello\":\"World\"}"

      input <-
        SUT.buildSqsInput
          (const $ return ())
          awsEnv
          "sqs-input-name"
          -- queue configured in yopa/config.yaml
          "http://localhost:47195/queue/message-queue"

      writeToInput input inputBytes
      (outputBytes, _deleteMsg) <- readFromInput input

      assertEqual "message roundtrip fails" outputBytes inputBytes
