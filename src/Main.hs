{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ViewPatterns    #-}

module Main where

import Run
import Image

import           Control.Lens
import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import qualified Data.ByteString              as BS
import           Data.ByteString.Builder      (Builder)
import           Data.Maybe                   (catMaybes)
import           Data.Monoid
import           Data.Text                    (Text )
import qualified Data.Text                    as T (pack) 
import           GHC.Exts
import qualified Network.AWS.CloudFormation   as CF
import           Network.AWS.Data             hiding ((.=))
import qualified Network.AWS.EC2              as EC2
import qualified Network.AWS.S3               as S3
import qualified Options.Applicative          as OP

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
importKey k = do
    ks <- map (view EC2.kpiKeyName) . view EC2.dkprKeyPairs <$> send EC2.describeKeyPairs
    unless (fst keyPair `elem` catMaybes ks) $ do
        info ("Importing key: " <> snd k)
        key <- liftIO $ BS.readFile (snd k)
        send_ $ EC2.importKeyPair (fst k) (Base64 key)

deleteKey :: KeyPair -> AWST IO ()
deleteKey (fst -> keyName) = do
    info ("Deleting key: " <> keyName)
    send_ (EC2.deleteKeyPair $ keyName)

deleteStack :: AWST IO CF.DeleteStackResponse
deleteStack = do
    info ("Deleting stack: " <> stackName)
    send $ CF.deleteStack stackName

createStack :: AWST IO CF.CreateStackResponse
createStack = do
    info ("Creating stack: " <> stackName)
    send $ CF.createStack stackName
        & CF.csTimeoutInMinutes ?~ 5
        & CF.csTemplateURL      ?~ templateUrl
        & CF.csParameters
            .~ [ CF.parameter & CF.pParameterKey   ?~ "KeyPairName"
                              & CF.pParameterValue ?~ fst keyPair
               , CF.parameter & CF.pParameterKey   ?~ "SystemName"
                              & CF.pParameterValue ?~ systemName
               ]
  where
    templateUrl = "https://s3-ap-southeast-2.amazonaws.com/"
                     <> templateBucketName <> "/"
                     <> templateObjectName

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

data Command = CreateStack
             | DeleteStack
             | RunInstance  String
             | KillInstance String

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

runInstanceParser :: OP.Parser Command
runInstanceParser = RunInstance <$> OP.argument OP.str (OP.metavar "INSTANCE-NAME")

killInstanceParser :: OP.Parser Command
killInstanceParser = KillInstance <$> OP.argument OP.str (OP.metavar "INSTANCE-NAME")

parseCommand :: OP.Parser Command
parseCommand = OP.subparser $
    OP.command "create-stack" (pure CreateStack `withInfo` "create cloud formation stack") <>
    OP.command "delete-stack" (pure DeleteStack `withInfo` "delete cloud formation stack") <>
    OP.command "run-instance" (runInstanceParser `withInfo` "launch an EC2 instance from a named image") <>
    OP.command "kill-instance" (killInstanceParser `withInfo` "save to an image and terminate an EC2 instance")

main :: IO ()
main = OP.execParser opts >>= \case
    CreateStack -> run (hoist lift createGeodesyMLDemoStack)
    DeleteStack -> run (hoist lift deleteGeodesyMLDemoStack)
    RunInstance name -> run (launch $ T.pack name)
    KillInstance name -> run (terminate $ T.pack name)
  where
    opts = OP.info
        (OP.helper <*> parseCommand)
        (OP.fullDesc <> OP.header "AWS Utilities")
