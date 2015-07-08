{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.AWS
import Data.Monoid
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString as BS
import Data.Text (Text)
import GHC.Exts
import Network.AWS.Data hiding ((.=))
import Network.AWS.CloudFormation
import Network.AWS.EC2
import Network.AWS.S3
import Network.HTTP.Conduit
import System.IO

default (Builder)

systemName :: Text
systemName = "GeodesyML Demo"

templateFileName :: FilePath
templateFileName = "stack-template.json"

templateBucketName :: Text
templateBucketName = "geodesyml-demo-stack-template"

templateObjectName :: Text
templateObjectName = "stack-template"

stackName :: Text
stackName = "test-stack"

keyPair :: (Text, FilePath)
keyPair = ("lazar@work", "/home/lazar/.ssh/id_rsa.pub")

createTemplateBucket :: AWST IO ()
createTemplateBucket = do
    info ("Creating bucket: " <> templateBucketName)
    send_ $ createBucket templateBucketName
        & cbCreateBucketConfiguration
        ?~ (createBucketConfiguration & cbcLocationConstraint ?~ Sydney)

deleteTemplateBucket :: AWST IO ()
deleteTemplateBucket = do 
    bs <- map (view bName) . view lbrBuckets <$> send listBuckets
    if elem templateBucketName bs
        then do info ("Deleting bucket: " <> templateBucketName)
                send_ $ deleteBucket templateBucketName
        else return ()

type URL = Text

withTemplate :: Env -> (URL -> AWST IO a) -> AWST IO a
withTemplate env f = do
    (releaseKey, _) <- allocate
        (runAWST env (createTemplateBucket >> uploadTemplate))
        (\_ -> void $ runAWST env (deleteTemplate >> deleteTemplateBucket))
    a <- f ("https://s3-ap-southeast-2.amazonaws.com/" <> templateBucketName <> "/" <> templateObjectName)
    release releaseKey
    return a

uploadTemplate :: AWST IO ()
uploadTemplate = do
    info ("Creating object: " <> templateObjectName)
    contents <- fromString <$> (liftIO $ readFile templateFileName)
    send_ $ putObject contents templateBucketName templateObjectName

deleteTemplate :: AWST IO ()
deleteTemplate = do
    bs <- map (view bName) . view lbrBuckets <$> send listBuckets
    when (elem templateBucketName bs) $ do
        os <- map (view oKey) . view lorContents <$> send (listObjects templateBucketName)
        when (elem templateObjectName os) $ do
            info ("Deleting object: " <> templateObjectName)
            send_ $ deleteObject templateBucketName templateObjectName

importKey :: AWST IO ()
importKey = do
    info ("Importing key: " <> snd keyPair)
    key <- liftIO $ BS.readFile (snd keyPair)
    send_ $ importKeyPair (fst keyPair) (Base64 key)

deleteKey :: AWST IO ()
deleteKey = do
    info ("Deleting key: " <> fst keyPair)
    send_ (deleteKeyPair $ fst keyPair)

deleteGeodesyMLDemoStack :: AWST IO DeleteStackResponse
deleteGeodesyMLDemoStack = do
    deleteTemplate
    deleteTemplateBucket
    deleteKey
    info ("Deleting stack: " <> stackName)
    send $ deleteStack stackName

createGeodesyMLDemoStack :: Env -> IO (Either Error CreateStackResponse)
createGeodesyMLDemoStack env = do
    runAWST env $ do
        createTemplateBucket
        uploadTemplate
        liftIO $ threadDelay 10000000
        createStackWithUrl ("https://s3-ap-southeast-2.amazonaws.com/" <> templateBucketName <> "/" <> templateObjectName)
  where
    createStackWithUrl url = do
        info ("Creating stack: " <> stackName)
        send $ createStack stackName
            & csTimeoutInMinutes ?~ 5
            & csTemplateURL ?~ url
            & csParameters  .~ [ parameter & pParameterKey   ?~ "KeyPairName"
                                        & pParameterValue ?~ (fst keyPair)
                            , parameter & pParameterKey   ?~ "SystemName"
                                        & pParameterValue ?~ systemName
                            ]

main :: IO ()
main = do
    logger <- newLogger Debug stdout
    env <- getEnv Sydney Discover <&> envLogger .~ logger
    manager <- newManager (conduitManagerSettings { managerResponseTimeout = Just 10000000 })
    let e = env & envManager .~ manager
    r <- runAWST e deleteGeodesyMLDemoStack
    print r
    -- r <- createGeodesyMLDemoStack env
    -- case r of
    --   Right r' -> putStrLn $ "Created stack" ++ show (view csrStackId r')
    --   Left e   -> putStrLn $ "Error: " ++ show e
