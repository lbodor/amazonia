{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Control.Concurrent           (threadDelay)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import           Data.Maybe                   (catMaybes)
import           Data.Monoid
import           Data.Text                    (Text)
import           GHC.Exts
import qualified Network.AWS.CloudFormation   as CF
import           Network.AWS.Data             hiding ((.=))
import qualified Network.AWS.EC2              as EC2
import qualified Network.AWS.S3               as S3
import           Network.HTTP.Conduit
import           Options.Applicative          ((<|>))
import qualified Options.Applicative          as OP
import           System.IO

default (Builder)

systemName :: Text
systemName = "GeodesyML Demo"

templateFileName :: FilePath
templateFileName = "stack-template.json"

templateBucketName :: Text
templateBucketName = "geodesyml-demo"

templateObjectName :: Text
templateObjectName = "stack-template"

stackName :: Text
stackName = "test-stack"

type KeyPair = (Text, FilePath)

keyPair :: KeyPair
keyPair = ("lazar@work", "/home/lazar/.ssh/id_rsa.pub")

createBucket :: Text -> AWST IO ()
createBucket name = do
    e <- bucketExists name
    unless e $ do
        info ("Creating bucket: " <> templateBucketName)
        send_ $ S3.createBucket templateBucketName
            & S3.cbCreateBucketConfiguration
            ?~ (S3.createBucketConfiguration & S3.cbcLocationConstraint ?~ Sydney)

deleteTemplateBucket :: AWST IO ()
deleteTemplateBucket = do
    e <- bucketExists templateBucketName
    when e $ do
        info ("Deleting bucket: " <> templateBucketName)
        send_ $ S3.deleteBucket templateBucketName

bucketExists :: Text -> AWST IO Bool
bucketExists name = do
    bucketNames <- map (view S3.bName) . view S3.lbrBuckets <$> send S3.listBuckets
    return $ elem name bucketNames

uploadTemplate :: AWST IO ()
uploadTemplate = do
    info ("Creating object: " <> templateObjectName)
    contents <- fromString <$> liftIO (readFile templateFileName)
    send_ $ S3.putObject contents templateBucketName templateObjectName

deleteTemplate :: AWST IO ()
deleteTemplate = do
    bs <- map (view S3.bName) . view S3.lbrBuckets <$> send S3.listBuckets
    when (templateBucketName `elem` bs) $ do
        info ("Deleting object: " <> templateObjectName)
        send_ $ S3.deleteObject templateBucketName templateObjectName

importKey :: KeyPair -> AWST IO ()
importKey keyPair = do
    ks <- map (view EC2.kpiKeyName) . view EC2.dkprKeyPairs <$> send EC2.describeKeyPairs
    unless (fst keyPair `elem` catMaybes ks) $ do
        info ("Importing key: " <> snd keyPair)
        key <- liftIO $ BS.readFile (snd keyPair)
        send_ $ EC2.importKeyPair (fst keyPair) (Base64 key)

deleteKey :: KeyPair -> AWST IO ()
deleteKey keyPair = do
    info ("Deleting key: " <> fst keyPair)
    send_ (EC2.deleteKeyPair $ fst keyPair)

deleteStack :: AWST IO CF.DeleteStackResponse
deleteStack = do
    info ("Deleting stack: " <> stackName)
    send $ CF.deleteStack stackName

createStack :: AWST IO CF.CreateStackResponse
createStack = do
        info ("Creating stack: " <> stackName)
        send $ CF.createStack stackName
            & CF.csTimeoutInMinutes ?~ 5
            & CF.csTemplateURL ?~ ("https://s3-ap-southeast-2.amazonaws.com/" <> templateBucketName <> "/" <> templateObjectName)
            & CF.csParameters  .~ [ CF.parameter & CF.pParameterKey   ?~ "KeyPairName"
                                                 & CF.pParameterValue ?~ fst keyPair
                                  , CF.parameter & CF.pParameterKey   ?~ "SystemName"
                                                 & CF.pParameterValue ?~ systemName
                                  ]

deleteGeodesyMLDemoStack :: AWST IO CF.DeleteStackResponse
deleteGeodesyMLDemoStack = do
    deleteTemplate
    deleteTemplateBucket
    deleteKey keyPair
    deleteStack

createGeodesyMLDemoStack :: AWST IO CF.CreateStackResponse
createGeodesyMLDemoStack = do
    createBucket templateBucketName
    uploadTemplate
    importKey keyPair
    createStack

run :: (Show a) => AWST IO a -> IO ()
run f = do
    logger <- newLogger Info stdout
    env <- getEnv Sydney Discover <&> envLogger .~ logger
    runAWST env f >>= print

data Mode = Create | Delete

argParser :: OP.Parser Mode
argParser = OP.flag'
        Delete (OP.short 'd' <> OP.long "delete" <> OP.help "delete the stack instead")
    <|> pure Create

main :: IO ()
main = OP.execParser opts >>= \case
    Create -> run createGeodesyMLDemoStack
    Delete -> run deleteGeodesyMLDemoStack
  where
    opts = OP.info
        (OP.helper <*> argParser)
        (OP.fullDesc <> OP.header "Create AWS Stack for GeodesyML Demo")
