module Run
    (run
    ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.AWS
import           Data.Text (Text)
import           System.IO

run :: (Show a) => AWST (ExceptT Text IO) a -> IO (Either Text (Either Error a))
run f = do
    logger <- newLogger Info stdout
    env <- getEnv Sydney Discover <&> envLogger .~ logger
    runExceptT $ runAWST env f
