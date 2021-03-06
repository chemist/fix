module Command.Add where

import System.Directory
import Data.Binary (encodeFile)
import System.FilePath
import Control.Monad.State

import Types
import Helpers

-- | create new layer 
createLayer :: Name -> ST ()
createLayer name = do
    fixDirectory <- getFixDirectory
    bucketName <- getBucketName
    liftIO $ createDirectoryIfMissing True $ fixDirectory </> bucketName </> "layers"
    addLayer name
    where
      addLayer :: Name -> ST ()
      addLayer n = do
          oldZip <- (rTree . stBucket) <$> get
          way <- getRoute
          layersPath <- getLayersPath
          let hash' = hash (n:way)
              cl = CLayer "" n hash'
          let newZip = attach cl oldZip
          modify $ \s -> s { stBucket = (stBucket s) { rTree = newZip }}
          liftIO $ encodeFile (layersPath </> hash') (Changes [] :: Changes DF)          


