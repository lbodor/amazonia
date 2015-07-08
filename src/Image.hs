{-# LANGUAGE OverloadedStrings #-}

module Image
    ( listImages
    ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import qualified Network.AWS.EC2              as EC2

listImages :: AWST IO ()
listImages = do
    images <- view EC2.dirImages <$> send req
    let creationDates = map (view EC2.iCreationDate) images
    liftIO $ print images
    liftIO $ print creationDates
  where
    req = EC2.describeImages
        & EC2.di2Owners  .~ ["self"]
        & EC2.di2Filters .~ [filters]
    filters = EC2.filter' "tag:Name" & EC2.fValues .~ ["Webserver"]

