{-# LANGUAGE ScopedTypeVariables #-}
module Helpers where

import System.Directory
import Data.Binary (decodeFile)
import System.FilePath
-- import System.Posix.Files
import Control.Monad.State
import Control.Monad.Writer hiding (First)
import Control.Exception.Base (try, SomeException, throw)
import qualified Crypto.Hash.MD5 as H
import qualified Data.ByteString.Char8 as H 
import qualified Data.ByteString.Lazy.Char8 as BL 
import qualified Data.ByteString as B 
import Control.Applicative
import Data.ByteString.Builder (word8Hex, Builder, toLazyByteString)
import Data.Word (Word8)

import Types
import Opts.Opts

path :: FilePath
path = "/Users/chemist/Develop/fix/tmp/"

fixDirectoryName :: Path
fixDirectoryName = ".fix"

fixStateName :: Path
fixStateName = "®««««««"

emptyFix :: Fix
emptyFix = Fix "." emptyBucket Normal

getFixDirectory :: ST Path
getFixDirectory = stFixDirectory <$> get

getBucket :: ST Bucket
getBucket = stBucket <$> get

getBucketName :: ST Name
getBucketName = rName <$> getBucket

getLayerName :: ST Name
getLayerName = index . rBase <$> getBucket 

getLayerHash :: ST (Maybe String)
getLayerHash = do
  v <- value . rTree <$> getBucket
  case v of
       Nothing -> return Nothing
       Just (CLayer _ _ h) -> return $ Just h

getRoute :: ST Route
getRoute = route . rTree <$> getBucket

isEmpty :: ST Bool
isEmpty = do
    rt <- rTree <$> getBucket
    case rt of
         (Empty, []) -> return $ True
         _ -> return $ False

isBucketAvailable :: Name -> ST Bool
isBucketAvailable name = elem name <$> getAllBuckets

getAllBuckets :: ST [Name]
getAllBuckets = do
    fixDirectory <- getFixDirectory
    files <- liftIO $ getDirectoryContents fixDirectory
    return $ bucketFilter files
    where
      bucketFilter :: [Path] -> [Path]
      bucketFilter = filter (`notElem` [".", "..", fixStateName])

getAllLayersFromBucket :: ST (Layer DF)
getAllLayersFromBucket = getLayers True

getParrentsLayersFromBucket :: ST (Layer DF)
getParrentsLayersFromBucket = getLayers False

getLayers :: Bool -> ST (Layer DF)
getLayers f = do
    base <- rBase <$> getBucket
    tr <- rTree <$> getBucket 
    let cvalues = 
          if f
             then values tr
             else maybe [] (values) (goUp tr)
    changes <- loadChanges cvalues
    return $ foldl (patch Apply) base changes
    where
      loadChanges :: [CLayer] -> ST [Changes DF]
      loadChanges = mapM loadChange 

emptyBucket :: Bucket
emptyBucket = Bucket "base" "empty set of layers" (emptyLayer "base") (Empty, [])

cleanBucket :: ST ()
cleanBucket = modify $ \s -> s { stBucket = emptyBucket }

emptyLayer :: Name -> Layer DF
emptyLayer n = Layer (n,[(".",D)])

hash :: Route -> String
hash xs = BL.unpack . toLazyByteString $ toHex $ B.unpack . H.hash . H.pack . concat $ xs
  where
  toHex :: [Word8] -> Builder
  toHex = foldl (\b w -> b <> word8Hex w) mempty

getLayersPath :: ST Path
getLayersPath = do
    fixDirectory <- getFixDirectory
    bucketName <- getBucketName
    return $ fixDirectory </> bucketName </> "layers" 

getWorkDirectory :: ST Path
getWorkDirectory = dropFileName . dropTrailingPathSeparator . stFixDirectory <$> get

updateState :: Options -> ST ()
updateState opts = modify $ 
    \s -> s { stVerbosity = optVerbosity opts }

cleanWorkSpace :: ST ()
cleanWorkSpace = liftIO . cleanDirectory =<< getWorkDirectory
    where
      cleanDirectory :: Path -> IO ()
      cleanDirectory filePath = do
          cont <- getDirectoryContents filePath
          forM_ (dotFilter cont) $ \x -> rm (filePath </> x) 

      dotFilter :: [Path] -> [Path]
      dotFilter = filter (`notElem` [".", "..", ".fix"])

      rm :: Path -> IO ()
      rm f = do
          temp <- try (removeFile f)
          case temp of
               Right _ -> return ()
               Left e -> do
                  isDir <- doesDirectoryExist f
                  unless isDir $ throw (e :: SomeException)
                  cleanDirectory f
                  removeDirectory f

loadChange :: CLayer -> ST (Changes DF)
loadChange (CLayer _ _ h) = do
    layersPath <- getLayersPath
    -- TODO: catch errors here
    either (\(_ :: SomeException) -> (Changes [])) id <$> (liftIO $ try $ decodeFile (layersPath </> h))

log :: Show a => a -> ST ()
log x = tell $ show x <> "\n"

msg :: Show a => a -> ST ()
msg = liftIO . print 

isWorkDirectoryClean :: ST Bool
isWorkDirectoryClean = do
    n <- getLayerName
    wd <- getWorkDirectory
    old <- getAllLayersFromBucket
    new <- liftIO $ dump n wd :: ST (Layer DF)
    return $ getPatch old new == Changes []

ifM :: Monad m => m Bool -> m () -> m () -> m ()
ifM getFlag bad good = do
    f <- getFlag
    if f then good else bad

whenClean :: Show a => ST () -> a -> ST ()
whenClean w m = ifM isWorkDirectoryClean (msg m) w




