module Command.Save where

import System.FilePath
import Control.Monad.State
import Data.Maybe
import Data.Binary (encodeFile)

import Prelude hiding (log)
import Helpers
import Types

saveWorkSpaceAsLayer :: ST ()
saveWorkSpaceAsLayer = do
    n <- getLayerName
    wd <- getWorkDirectory
    old <- getParrentsLayersFromBucket 
    log "old: "
    log old
    new <- liftIO $ dump n wd :: ST (Layer DF)
    layersPath <- getLayersPath
    h <- getLayerHash
    let changes = getPatch (sortLayer old) (sortLayer new)
    log "changes: "
    log changes
    case (h, changes) of
         (_, Changes [])  -> return ()
         (Nothing, _)  -> msg "Changes not saved, add layer first"
         (Just hash', _) -> liftIO $ encodeFile (layersPath </> hash') changes
