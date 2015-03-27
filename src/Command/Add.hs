module Command.Add where

import Data.Tree (Name)
import Data.Types
import Data.Layer 
import Helpers
import qualified Data.Tree as Tree

import System.Directory
import System.FilePath
-- import System.Posix.Files
import Control.Monad.State
import Control.Applicative


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
          let newZip = Tree.attach cl oldZip
          modify $ \s -> s { stBucket = (stBucket s) { rTree = newZip }}
          liftIO $ save (layersPath </> hash') (Changes [] :: Changes Body)          


