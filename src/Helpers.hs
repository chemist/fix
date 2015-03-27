{-# LANGUAGE ScopedTypeVariables #-}
module Helpers where

import Control.Applicative
import System.Directory
import System.FilePath
-- import System.Posix.Files
import Control.Monad.State
import Control.Monad.Writer hiding (First)
import Control.Exception.Base (try, SomeException, throw)
import qualified Crypto.Hash.MD5 as H
import qualified Data.ByteString.Char8 as H 
import qualified Data.ByteString.Lazy.Char8 as BL 
import qualified Data.ByteString as B 
import Data.ByteString.Builder (word8Hex, Builder, toLazyByteString)
import Data.Word (Word8)

import Data.Layer 
import Data.Tree (Route, Name)
import Data.Types
import Opts.Opts
import qualified Data.Tree as Tree

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
getLayerName = Tree.index . rBase <$> getBucket 

getLayerHash :: ST (Maybe String)
getLayerHash = do
  v <- Tree.value . rTree <$> getBucket
  case v of
       Nothing -> return Nothing
       Just (CLayer _ _ h) -> return $ Just h

getRoute :: ST Route
getRoute = Tree.route . rTree <$> getBucket

isEmpty :: ST Bool
isEmpty = do
    rt <- rTree <$> getBucket
    case rt of
         (Tree.Empty, []) -> return $ True
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

getAllLayersFromBucket :: ST (Layer Body)
getAllLayersFromBucket = getLayers True

getParrentsLayersFromBucket :: ST (Layer Body)
getParrentsLayersFromBucket = getLayers False

getLayers :: Bool -> ST (Layer Body)
getLayers f = do
    base <- rBase <$> getBucket
    tr <- rTree <$> getBucket 
    let cvalues = 
          if f
             then Tree.values tr
             else maybe [] (Tree.values) (Tree.goUp tr)
    changes <- loadChanges cvalues
    return $ foldl (patch Apply) base changes
    where
      loadChanges :: [CLayer] -> ST [Changes Body]
      loadChanges = mapM loadChange 

emptyBucket :: Bucket
emptyBucket = Bucket "base" "empty set of layers" (emptyLayer "base") (Tree.Empty, [])

cleanBucket :: ST ()
cleanBucket = modify $ \s -> s { stBucket = emptyBucket }

emptyLayer :: Name -> Layer Body
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

loadChange :: CLayer -> ST (Changes Body)
loadChange (CLayer _ _ h) = do
    layersPath <- getLayersPath
    -- TODO: catch errors here
    either (\(_ :: SomeException) -> (Changes [])) id <$> (liftIO $ try $ restore (layersPath </> h))

log :: Show a => a -> ST ()
log x = tell $ show x <> "\n"

msg :: Show a => a -> ST ()
msg = liftIO . print 

isWorkDirectoryClean :: ST Bool
isWorkDirectoryClean = do
    n <- getLayerName
    wd <- getWorkDirectory
    old <- getAllLayersFromBucket
    new <- liftIO $ load n wd :: ST (Layer Body)
    return $ getPatch old new == Changes []

ifM :: Monad m => m Bool -> m () -> m () -> m ()
ifM getFlag bad good = do
    f <- getFlag
    if f then good else bad

whenClean :: Show a => ST () -> a -> ST ()
whenClean w m = ifM isWorkDirectoryClean (msg m) w




