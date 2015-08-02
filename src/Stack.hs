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

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import           Data.Maybe                   (catMaybes)
import           Data.Monoid
import           Data.Text                    (Text )
import qualified Network.AWS.CloudFormation   as CF
import           Network.AWS.Data             hiding ((.=))
import qualified Network.AWS.EC2              as EC2

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

createStack :: Text -> Text -> [(Text, Text)] -> AWST IO CF.CreateStackResponse
createStack stackName templateUrl parameters = do
    info ("Creating stack: " <> stackName)
    send $ CF.createStack stackName
        & CF.csTimeoutInMinutes ?~ 5
        & CF.csTemplateURL      ?~ templateUrl
        & CF.csParameters       .~ map cfParameter parameters
  where
     cfParameter (name, value) = CF.parameter
      & CF.pParameterKey   ?~ name
      & CF.pParameterValue ?~ value



