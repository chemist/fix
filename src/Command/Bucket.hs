module Command.Bucket where

import System.Directory
import System.FilePath
import Control.Monad.State
import Control.Applicative
import Data.Binary (encodeFile, decodeFile)

import Prelude hiding (log)
import Types
import Helpers

viewOrCreateOrSwitchBucket :: Name -> ST ()
viewOrCreateOrSwitchBucket "" = do
    buckets <- getAllBuckets 
    liftIO $ forM_ buckets putStrLn
viewOrCreateOrSwitchBucket name = do
    buckets <- getAllBuckets
    ifM ((/= name) <$> getBucketName)
        (if elem name buckets
           then whenClean switch ("can't switch bucket, work directory has unsaved files" :: String)
           else whenClean create ("can't create bucket, work directory has unsaved files" :: String)
        )
        (return ()) -- current name, nothing to do
    where
      switch = do
          log ("try switch bucket" :: String)
          current <- getBucketName
          if current == name
             then return ()
             else do
                  log ("switch bucket" :: String)
                  saveBucket
                  cleanWorkSpace
                  restoreBucket name
                  restoreWorkSpaceFromBucket
      create = do
          log ("create bucket" :: String)
          saveBucket
          cleanWorkSpace
          fixDirectory <- getFixDirectory
          liftIO $ createDirectoryIfMissing True $ fixDirectory </> name
          addBucket name
          saveBucket
          where
            addBucket :: Name -> ST () 
            addBucket n = modify $ \s -> 
              s { stBucket = emptyBucket { rBase = emptyLayer n
                                         , rName = n 
                                         } 
                }

restoreBucket :: Name -> ST ()
restoreBucket name = do
    log ("restore bucket" :: String)
    fixDirectory <- getFixDirectory
    available <- isBucketAvailable name
    if available
       then do
           bucket <- liftIO $ decodeFile $ fixDirectory </> name </> "bucket"
           modify $ \s -> s { stBucket = bucket }
       else msg ("bucket not found" :: String)

restoreWorkSpaceFromBucket :: ST ()
restoreWorkSpaceFromBucket = do
    log ("restore work space from bucket" :: String)
    layer <- getAllLayersFromBucket
    log ("layer: " :: String)
    log layer
    work <- getWorkDirectory
    liftIO $ restore work layer

saveBucket :: ST ()
saveBucket = do
    log ("save bucket" :: String)
    bucketName <- getBucketName
    bucket <- getBucket
    fixDirectory <- getFixDirectory
    liftIO $ encodeFile (fixDirectory </> bucketName </> "bucket") bucket


