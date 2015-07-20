module Command.Clean where

import Control.Monad.State
import Types
import Helpers
import Prelude hiding (log)

clean :: ST ()
clean = do
    log ("clean workspace" :: String)
    cleanWorkSpace
    layer <- getAllLayersFromBucket
    work <- getWorkDirectory
    liftIO $ restore work layer


