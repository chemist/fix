module Command.Save where

import System.FilePath
import Control.Monad.State
import Data.Maybe
import Data.Binary (encodeFile)

import Prelude hiding (log)
import Helpers
import Types

saveWorkSpaceAsLayer :: ST ()
saveWorkSpaceAsLayer = ifM isNotRendered good bad
  where
  bad = msg "Can't save when rendered. First do fix clean."
  good = do
    n <- getLayerName
    wd <- getWorkDirectory
    old <- getParrentsLayersFromBucket 
    log ("old: " :: String)
    log old
    new <- liftIO $ dump n wd :: ST (Layer DF)
    layersPath <- getLayersPath
    h <- getLayerHash
    let changes = getPatch (sortLayer old) (sortLayer new)
    log ("changes: " :: String)
    log changes
    log ("hash: " :: String)
    log h
    case (h, changes) of
         (_, Changes [])  -> return ()
         (Nothing, _)  -> msg ("Changes not saved, add layer first" :: String)
         (Just hash', _) -> liftIO $ encodeFile (layersPath </> hash') changes
