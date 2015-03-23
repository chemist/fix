{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Directory
import System.FilePath
-- import System.Posix.Files
import Control.Monad.State
import Control.Monad.Writer
import Data.Binary (decodeFile, encodeFile, Binary)
import GHC.Generics (Generic)
import Text.Printf
import Control.Applicative
import Control.Exception.Base (try, SomeException, throw)
import qualified Crypto.Hash.MD5 as H
import qualified Data.ByteString.Char8 as H 
import qualified Data.ByteString.Lazy.Char8 as BL 
import qualified Data.ByteString as B 
import Data.ByteString.Builder (word8Hex, Builder, toLazyByteString)
import Data.Word (Word8)
import Data.Either (rights, lefts)
import Data.List (sortBy)

import Prelude hiding (log)
import Layer 
import Tree (Route, Name)
import qualified Tree

import Opts.Opts


path :: FilePath
path = "/Users/chemist/Develop/fix/tmp/"

fixDirectoryName :: Path
fixDirectoryName = ".fix"

fixStateName :: Path
fixStateName = "®««««««"

emptyFix :: Fix
emptyFix = Fix "." emptyBucket Normal

readState :: Path -> IO Fix
readState fixDirectory = do
    let fixState = fixDirectory </> fixStateName
    stateAvailable <- doesFileExist fixState
    if stateAvailable
       then setFixDirectory fixDirectory <$> decodeFile fixState
       else return $ setFixDirectory fixDirectory emptyFix

setFixDirectory :: Path -> Fix -> Fix
setFixDirectory filePath f = f { stFixDirectory = filePath }

writeState :: Path -> (String, Fix) -> IO ()
writeState fixDirectory (str, st) = do
    encodeFile (fixDirectory </> fixStateName) st
    when (stVerbosity st == Verbose) $ do
        print st
        putStr "\n   Usage log: \n"
        putStr str

main :: IO ()
main = do
    command <-  parseOptions 
    let fixDirectory = optFixPath command </> fixDirectoryName
    fixDirectoryAvailable <-  doesDirectoryExist $ fixDirectory
    unless fixDirectoryAvailable $ runInit (optFixPath command) (optCommand command)
    writeState fixDirectory =<< runST (execute command) =<< readState fixDirectory
--     createDirectoryIfMissing True realp

execute :: Options -> ST ()
execute opts = do
    updateState opts
    log $ optCommand opts
    run (optCommand opts)

runInit :: Path -> Command -> IO ()
runInit fixPath' (Command Init _) = do
    createDirectoryIfMissing False $ fixPath' </> fixDirectoryName
    createDirectoryIfMissing False $ fixPath' </> fixDirectoryName </> "base"
    createDirectoryIfMissing False $ fixPath' </> fixDirectoryName </> "base" </> "layers"
runInit _ _ = error "Cant found fix directory, try fix init, or fix -f path to fix directory"

    
run :: Command -> ST ()
run (Command (Add LayerContext) name) = do
    ifM isEmpty
      (createLayer name >> goByRoute name)
      (createLayer name)
run (Command BucketOpt name) = do
    viewOrCreateOrSwitchBucket name
run (Command Init _) = return ()
run (Command Save _) = do
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
run (Command (Go (ByRoute route')) _) = whenClean (goRoute route' >> cleanWorkSpace >> restoreWorkSpaceFromBucket) "you must save work directory"
run (Command (Go DUp) _) = whenClean (goUp >> cleanWorkSpace >> restoreWorkSpaceFromBucket) "you must save work directory"
run (Command (Go DDown) _) = whenClean (goDown >> cleanWorkSpace >> restoreWorkSpaceFromBucket) "you must save work directory"
run _ = liftIO $ printf "command not realizaded"

goByRoute :: Name -> ST ()
goByRoute name = do
    r <- getRoute 
    run (Command (Go (ByRoute $ r <> [name])) "")

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
    return $ sortLayer old == sortLayer new

sortLayer :: Layer Body -> Layer Body
sortLayer (Layer (x, xs)) = Layer (x, sortBy fun xs)
  where
  fun a b = compare (fst a) (fst b)



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

viewOrCreateOrSwitchBucket :: Name -> ST ()
viewOrCreateOrSwitchBucket "" = do
    buckets <- getAllBuckets 
    liftIO $ forM_ buckets putStrLn
viewOrCreateOrSwitchBucket name = do
    buckets <- getAllBuckets
    ifM ((/= name) <$> getBucketName)
        (return ()) -- current name, nothing to do
        (if elem name buckets
           then whenClean switch "can't switch bucket, work directory has unsaved files"
           else whenClean create "can't create bucket, work directory has unsaved files"
        )
    where
      switch = do
          log "try switch bucket"
          current <- getBucketName
          if current == name
             then return ()
             else do
                  log "switch bucket"
                  saveBucket
                  cleanWorkSpace
                  restoreBucket name
                  restoreWorkSpaceFromBucket
      create = do
          log "create bucket"
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

ifM :: Monad m => m Bool -> m () -> m () -> m ()
ifM getFlag bad good = do
    f <- getFlag
    if f then good else bad

whenClean :: Show a => ST () -> a -> ST ()
whenClean w m = ifM isWorkDirectoryClean (msg m) w

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
      loadChanges xs = do
          layersPath <- getLayersPath
          changes <- liftIO $ forM xs (loadChange layersPath)
          when (not . null $ lefts changes) $ log $ "ERROR: " <> show (lefts changes)
          return $ rights changes
      loadChange :: Path -> CLayer -> IO (Either SomeException (Changes Body))
      loadChange lp (CLayer _ _ h) = try $ restore (lp </> h)

restoreBucket :: Name -> ST ()
restoreBucket name = do
    log "restore bucket"
    fixDirectory <- getFixDirectory
    available <- isBucketAvailable name
    if available
       then do
           bucket <- liftIO $ restore $ fixDirectory </> name </> "bucket"
           modify $ \s -> s { stBucket = bucket }
       else msg "bucket not found"

restoreWorkSpaceFromBucket :: ST ()
restoreWorkSpaceFromBucket = do
    log "restore work space from bucket"
    layer <- getAllLayersFromBucket
    log "layer: "
    log layer
    work <- getWorkDirectory
    liftIO $ dump work layer

saveBucket :: ST ()
saveBucket = do
    log "save bucket"
    bucketName <- getBucketName
    bucket <- getBucket
    fixDirectory <- getFixDirectory
    liftIO $ save (fixDirectory </> bucketName </> "bucket") bucket

isBucketAvailable :: Name -> ST Bool
isBucketAvailable name = elem name <$> getAllBuckets

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

getFixDirectory :: ST Path
getFixDirectory = stFixDirectory <$> get

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

goUp :: ST ()
goUp = do
    log "go up"
    bucket <- getBucket
    case Tree.goLevel (rTree bucket) of
         Nothing -> return ()
         Just new -> modify $ \s -> s { stBucket = bucket { rTree = new }}

goDown :: ST ()
goDown = do
    log "go down"
    bucket <- getBucket
    case Tree.goUp (rTree bucket) of
         Nothing -> return ()
         Just new -> modify $ \s -> s { stBucket = bucket { rTree = new }}

goRoute :: Route -> ST ()
goRoute route' = do
    log "go route"
    bucket <- getBucket
    modify $ \s -> s { stBucket = bucket { rTree = (Tree.goClosest route' (rTree bucket)) }}

type ST = WriterT String (StateT Fix IO)

runST :: ST () -> Fix -> IO (String, Fix)
runST = runStateT . execWriterT

data Fix = 
  Fix { stFixDirectory    :: Path
      , stBucket          :: Bucket
      , stVerbosity       :: Verbosity
      } deriving (Eq, Generic)

instance Binary Fix

instance Show Fix where
    show x = "\nfix directory: " <> (show $ stFixDirectory x) 
           <> "\nbucket: " <> (rName . stBucket $ x ) <> "\n"
           <> "way: " <> (route . Tree.route . rTree . stBucket $ x ) <> "\n"
           <> (show $ (rTree . stBucket) x) <> "\n"
      where
        route [] = "-"
        route xs = foldl1 (\a b -> a <> "." <> b) xs
           -- <> "history: \n" <> (unlines $ map show (take 10 $ stHistory x))
           
