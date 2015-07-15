module Run
    (run
    ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.AWS hiding (getEnv)
import qualified Control.Monad.Trans.AWS as AWS (getEnv)
import           Data.Text               (Text)
import           System.Environment      (getEnv)
import           System.IO

run :: (Show a) => AWST (ExceptT Text IO) a -> IO (Either Text (Either Error a))
run f = do
    logger <- newLogger Info stdout
    region <- read <$> liftIO (getEnv "AWS_REGION")
    env <- AWS.getEnv region Discover <&> envLogger .~ logger
    runExceptT $ runAWST env f
