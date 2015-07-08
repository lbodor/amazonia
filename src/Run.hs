module Run
    (run
    ) where

import           Control.Lens
import           Control.Monad.Trans.AWS
import           System.IO

run :: (Show a) => AWST IO a -> IO ()
run f = do
    logger <- newLogger Info stdout
    env <- getEnv Sydney Discover <&> envLogger .~ logger
    runAWST env f >>= print
