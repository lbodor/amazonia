{-# LANGUAGE OverloadedStrings #-}

module Run
    (run
    ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.AWS hiding (getEnv)
import qualified Control.Monad.Trans.AWS as AWS (getEnv)
import           Control.Retry           (RetryPolicy(..))
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T (pack)
import qualified Data.Text.IO            as T (putStrLn)
import           System.Environment      (getEnv)
import           System.IO

run :: (Show a) => AWST (ExceptT Text IO) a -> IO ()
run f = do
    logger <- newLogger Info stdout
    region <- read <$> liftIO (getEnv "AWS_REGION")
    env    <- AWS.getEnv region Discover
                <&> envLogger      .~ logger
                <&> envRetryPolicy ?~ retryPolicy
    result <- runExceptT $ runAWST env f
    case join (over (_Right . _Left) (T.pack . show) result) of
        Right r -> T.putStrLn ("OK: " <> (T.pack . show) r)
        Left e  -> T.putStrLn ("Error: " <> e)
  where
    retryPolicy = RetryPolicy $ \n ->
      if n < 5 then Just 50000 else Nothing
