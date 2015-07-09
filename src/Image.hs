{-# LANGUAGE OverloadedStrings #-}

module Image
    ( launch
    ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Trans.AWS
import           Data.Function           (on)
import           Data.List
import           Data.Text (Text)
import qualified Network.AWS.EC2         as EC2

latestImageByName :: Text -> AWST IO Text
latestImageByName name = do
    images <- view EC2.dirImages <$> send req
    let pairs = map (view EC2.iImageId &&& view EC2.iCreationDate) images
    return $ fst $ head $ sortByDescendingDate pairs
  where
    req = EC2.describeImages
        & EC2.di2Owners  .~ ["self"]
        & EC2.di2Filters .~ [filters]
    filters = EC2.filter' "tag:Name" & EC2.fValues .~ [name]
    sortByDescendingDate = sortBy (flip compare `on` snd)

launch :: Text -> AWST IO ()
launch name = do
    imageId   <- latestImageByName name
    instances <- view EC2.rirInstances <$> send (runInstanceRq imageId)
    send_ $ createTagsRq $ map (view EC2.i1InstanceId) instances
  where
    runInstanceRq imageId = EC2.runInstances imageId 1 1
        & EC2.riInstanceType ?~ EC2.T2_Medium

    createTagsRq instanceIds = EC2.createTags
        & EC2.ct1Resources .~ instanceIds
        & EC2.ct1Tags .~ [(EC2.tag "Name" name)]

