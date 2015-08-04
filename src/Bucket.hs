{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Bucket (
      uploadObjectFromFile
    , uploadObject
    , deleteObject
    , createBucket
    , deleteBucket
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
-- import qualified Control.Monad.Trans.AWS      as AWS
import           Control.Monad.Trans.AWS      (AWST, send, send_, info, envRegion)
import           Data.Monoid
import           Data.Text                    (Text)
import           GHC.Exts
import qualified Network.AWS.S3               as S3

bucketExists :: Text -> AWST IO Bool
bucketExists b = do
    bs <- map (view S3.bName) . view S3.lbrBuckets <$> send S3.listBuckets
    return $ b `elem` bs

createBucket :: Text -> AWST IO ()
createBucket name = do
    e <- bucketExists name
    unless e $ do
        info ("Creating bucket: " <> name)
        region <- asks (view envRegion)
        send_ (S3.createBucket name & S3.cbCreateBucketConfiguration ?~ bucketCfg region)
  where
    bucketCfg region = S3.createBucketConfiguration
      & S3.cbcLocationConstraint ?~ region

deleteBucket :: Text -> AWST IO ()
deleteBucket name = do
    e <- bucketExists name
    when e $ do
        info ("Deleting bucket: " <> name)
        send_ (S3.deleteBucket name)

uploadObjectFromFile :: Text -> Text -> FilePath -> AWST IO ()
uploadObjectFromFile bucket key file =
    liftIO (readFile file) >>= uploadObject bucket key

uploadObject :: Text -> Text -> String -> AWST IO ()
uploadObject bucket key object = do
    info ("Creating object: " <> key)
    send_ $ S3.putObject (fromString object) bucket key

deleteObject :: Text -> Text -> AWST IO ()
deleteObject bucket key = do
    bs <- map (view S3.bName) . view S3.lbrBuckets <$> send S3.listBuckets
    when (bucket `elem` bs) $ do
        info ("Deleting object: " <> key)
        send_ $ S3.deleteObject bucket key
