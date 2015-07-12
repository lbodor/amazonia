{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Image
    ( createImage
    , launch
    ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Trans
import           Control.Monad.Trans.AWS
import           Data.Function           (on)
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.AWS.EC2         as EC2
import           Safe

latestImageByName :: Text -> AWST IO (Maybe Text)
latestImageByName name = do
    images <- view EC2.dirImages <$> send req
    let pairs = map (view EC2.iImageId &&& view EC2.iCreationDate) images
    return $ fst <$> headMay (sortByDescendingDate pairs)
  where
    req = EC2.describeImages
        & EC2.di2Owners  .~ ["self"]
        & EC2.di2Filters .~ [filters]
    filters = EC2.filter' "tag:Name" & EC2.fValues .~ [name]
    sortByDescendingDate = sortBy (flip compare `on` snd)

launch :: Text -> AWST IO ()
launch name = do
    latestImageByName name >>= maybe
        (do liftIO $ putStrLn "image not found"
            return ())
        (\imageId -> do
            instances <- view EC2.rirInstances <$> send (runInstanceRq imageId)
            let instanceIds = map (^. EC2.i1InstanceId) instances
            assignName instanceIds name
            reservations <- view EC2.dirReservations <$> await EC2.instanceRunning (describeInstancesRq instanceIds)
            let runningInstances = concatMap (^. EC2.rInstances) reservations
            liftIO $ print $ map (^. EC2.i1PublicIpAddress) runningInstances
            return ())
            
  where
    runInstanceRq imageId = EC2.runInstances imageId 1 1
        & EC2.riInstanceType ?~ EC2.T2_Medium

    describeInstancesRq instanceIds = EC2.describeInstances
        & EC2.di1InstanceIds .~ instanceIds

assignName :: [Text] -> Text -> AWST IO ()
assignName resourceIds name =
    send_ $ EC2.createTags
        & EC2.ct1Resources .~ resourceIds
        & EC2.ct1Tags .~ [(EC2.tag "Name" name)]

timestamp :: IO Text
timestamp = do
    t <- (T.pack . show) <$> getPOSIXTime
    return (T.replace " " "_" t)

type ImageId = Text

createImage :: Text -> AWST IO (Either Text ImageId)
createImage instanceName = do
    reservations <- view EC2.dirReservations <$> send describeInstancesRq
    let instances = concatMap (^. EC2.rInstances) reservations
        instanceIds = map (^. EC2.i1InstanceId) instances

    case instanceIds of
        [instanceId] -> do
            t <- liftIO timestamp
            liftIO $ print t
            let rs = send (EC2.createImage instanceId $ instanceName <> "-" <> t)
            (view EC2.cirImageId <$> rs) >>= maybe
                (return $ Left "EC2.createImage did not return an image id.")
                (\imageId -> do assignName [imageId] instanceName
                                return $ Right imageId)
        []           -> return $ Left "instance name not found"
        _            -> return $ Left "instance name not unique"
  where
    describeInstancesRq = EC2.describeInstances
        & EC2.di1Filters .~ filters

    filters = [EC2.filter' "tag:Name" & EC2.fValues .~ [instanceName]
              ,EC2.filter' "instance-state-name" & EC2.fValues .~ ["running"]
              ]

    
    

