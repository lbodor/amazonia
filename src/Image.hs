{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Image
    ( createImage
    , latestImageByName
    , launch
    ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Morph
-- import           Control.Monad.Trans
import           Control.Monad.Trans.AWS
import           Data.Function           (on)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.AWS.EC2         as EC2
-- import           Safe

latestImageByName :: Text -> AWST (ExceptT Text IO) Text
latestImageByName name = do
    images <- view EC2.dirImages <$> send req
    let pairs = map (view EC2.iImageId &&& view EC2.iCreationDate) images
    case sortByDescendingDate pairs of
        []    -> lift (throwError "no such image")
        (p:_) -> return (fst p)
  where
    req = EC2.describeImages
        & EC2.di2Owners  .~ ["self"]
        & EC2.di2Filters .~ [filters]
    filters = EC2.filter' "tag:Name" & EC2.fValues .~ [name]
    sortByDescendingDate = sortBy (flip compare `on` snd)

launch :: Text -> AWST (ExceptT Text IO) Text
launch name = do
    imageId <- latestImageByName name
    instance' <- runInstance imageId
    let instanceId = view EC2.i1InstanceId instance'
    hoist lift (assignName instanceId name)
    reservations <- view EC2.dirReservations <$> waitForInstance instanceId
    let runningInstances = concatMap (^. EC2.rInstances) reservations
        publicIPs = catMaybes (map (^. EC2.i1PublicIpAddress) runningInstances)

    case publicIPs of
        []   -> lift (throwError "no IP address returned")
        [ip] -> return ip
        _    -> lift (throwError "multiple IP addresses returned")
  where
    waitForInstance instanceId = await EC2.instanceRunning (describeInstanceRq instanceId)

    describeInstanceRq instanceId = EC2.describeInstances
        & EC2.di1InstanceIds .~ [instanceId]

runInstance :: Text -> AWST (ExceptT Text IO) EC2.Instance
runInstance imageId = do
    instances <- view EC2.rirInstances <$> send runInstanceRq
    case instances of
        [i] -> return i
        []  -> lift (throwError "runInstance returned zero instances")
        _   -> lift (throwError "runInstance returned multiple instances")
  where
    runInstanceRq = EC2.runInstances imageId 1 1
        & EC2.riInstanceType ?~ EC2.T2_Medium

assignName :: Text -> Text -> AWST IO ()
assignName resourceIds name =
    send_ $ EC2.createTags
        & EC2.ct1Resources .~ [resourceIds]
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
                (\imageId -> do assignName imageId instanceName
                                return $ Right imageId)
        []           -> return $ Left "instance name not found"
        _            -> return $ Left "instance name not unique"
  where
    describeInstancesRq = EC2.describeInstances
        & EC2.di1Filters .~ filters

    filters = [EC2.filter' "tag:Name" & EC2.fValues .~ [instanceName]
              ,EC2.filter' "instance-state-name" & EC2.fValues .~ ["running"]
              ]

    
    

