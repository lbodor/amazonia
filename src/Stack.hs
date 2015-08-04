{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Stack
    ( uploadKeyPair
    , deleteKeyPair
    , createStack
    , deleteStack
    ) where

import Bucket
import Run

import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Morph          (hoist)
-- import           Control.Monad.Reader
import           Control.Monad.Trans.AWS      (AWST, send, send_, info)
import           Control.Monad.Trans.Resource (allocate, release)
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import           Data.Maybe                   (catMaybes)
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T (pack)
import qualified Network.AWS.CloudFormation   as CF
import           Network.AWS.Data             hiding ((.=))
import qualified Network.AWS.EC2              as EC2
import           System.Environment           (getEnv)

default (Builder)

uploadKeyPair :: Text -> FilePath -> AWST IO ()
uploadKeyPair keyName keyFile = do
    ks <- map (view EC2.kpiKeyName) . view EC2.dkprKeyPairs <$> send EC2.describeKeyPairs
    unless (keyName `elem` catMaybes ks) $ do
        info ("Importing key: " <> keyFile)
        key <- liftIO $ BS.readFile keyFile
        send_ $ EC2.importKeyPair keyName (Base64 key)

deleteKeyPair :: Text -> AWST IO ()
deleteKeyPair key = do
    info ("Deleting key: " <> key)
    send_ (EC2.deleteKeyPair key)

deleteStack :: Text -> AWST IO CF.DeleteStackResponse
deleteStack stackName = do
    info ("Deleting stack: " <> stackName)
    send $ CF.deleteStack stackName

createStack :: Text -> FilePath -> [(Text, Text)] -> AWST IO CF.CreateStackResponse
createStack stackName template parameters = do
    bucket <- T.pack <$> (liftIO $ getEnv "AWS-BUCKET")
    let key = stackName <> "-stacktemplate"
    (releaseKey, templateUrl) <- allocate
        (do run (hoist lift $ uploadObjectFromFile bucket key template)
            return ("https://s3-ap-southeast-2.amazonaws.com/" <> bucket <> "/" <> key))
        (\_ -> run (hoist lift $ deleteObject bucket key))
    info ("Creating stack: " <> stackName)
    s <- send $ CF.createStack stackName
        & CF.csTimeoutInMinutes ?~ 5
        & CF.csTemplateURL      ?~ templateUrl
        & CF.csParameters       .~ map cfParameter parameters
    release releaseKey
    return s
  where
     cfParameter (name, value) = CF.parameter
      & CF.pParameterKey   ?~ name
      & CF.pParameterValue ?~ value



