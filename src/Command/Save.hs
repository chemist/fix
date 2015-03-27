module Command.Save where

import System.FilePath
import Control.Monad.State
import Data.Maybe

import Prelude hiding (log)
import Data.Layer 
import Data.Types
import Helpers

saveWorkSpaceAsLayer :: ST ()
saveWorkSpaceAsLayer = do
    n <- getLayerName
    wd <- getWorkDirectory
    old <- getParrentsLayersFromBucket 
    log "old: "
    log old
    new <- liftIO $ load n wd :: ST (Layer Body)
    layersPath <- getLayersPath
    h <- getLayerHash
    let changes = getPatch (sortLayer old) (sortLayer new)
    log "changes: "
    log changes
    case (h, changes) of
         (_, Changes [])  -> return ()
         (Nothing, _)  -> msg "Changes not saved, add layer first"
         (Just hash', _) -> liftIO $ save (layersPath </> hash') changes
