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
import Control.Exception.Base (try, throw, SomeException)
import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (log)
import Layer 
import Tree (Route, Name)
import qualified Tree

import Opts.Opts


path :: FilePath
path = "/Users/chemist/Develop/fix/tmp/"

fixDirectoryName :: Path
fixDirectoryName = ".fix"

fixFile :: Path
fixFile = "®««««««"

layerPrefix :: Path
layerPrefix = ""

setPrefix :: Path
setPrefix = ""

emptyFix :: Fix
emptyFix = Fix "." emptyLayers Map.empty [] Normal

readState :: Path -> IO Fix
readState fixDirectory = do
    let fixState = fixDirectory </> fixFile
    stateAvailable <- doesFileExist fixState
    if stateAvailable
       then setFixDirectory fixDirectory <$> decodeFile fixState
       else return $ setFixDirectory fixDirectory emptyFix

setFixDirectory :: Path -> Fix -> Fix
setFixDirectory filePath f = f { stFixDirectory = filePath }

writeState :: Path -> (String, Fix) -> IO ()
writeState fixDirectory (str, st) = do
    encodeFile (fixDirectory </> fixFile) st
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
    createDirectoryIfMissing False $ fixPath' </> fixDirectoryName </> setPrefix <> "base"
runInit _ _ = error "Cant found fix directory, try fix init, or fix -f path to fix directory"

    
run :: Command -> ST ()
run (Command (Add context) name) = do
    create context name
run (Command Init _) = return ()
run (Command Save _) = do
    n <- getLayerName
    wd <- getWorkDirectory
    pos <- stPosition <$> get
    new <- liftIO $ load n wd :: ST (Layer Body)
    old <- getWorkState pos
    fd <- getFixDirectory
    let changes = getPatch old new
    liftIO $ save (makeLayerPath fd pos) changes
    return ()
{--
run (Command Switch OLayer layerName) = do
    isLayer <- doesExists OLayer layerName
    if isLayer
       then do
           -- switch to layer code
           modify $ \s -> s { stCurrentLayer = Just layerName }
           clean Work ""
           clone OLayer Work layerName ""
       else liftIO $ printf "layer does not exist, try create new layer"
run (Command Save _ _) = do
    Just layerName <- stCurrentLayer <$> get
    clean OLayer layerName
    clone Work OLayer "" layerName 
run (Command Pwd _ _) = do
    currentLayer <- stCurrentLayer <$> get
    liftIO . printf $ maybe "\n" (<> "\n") currentLayer
run (Command View Work _) = do
    liftIO $ printf "\nLayers: \n"
    fixDirectory <- getFixDirectory
    layers <- filter onlyLayers <$> (liftIO $ getDirectoryContents fixDirectory)
    forM_ layers (liftIO . printf "%s\n")
    where
      onlyLayers ('®':'®':_) = True
      onlyLayers _ = False
--}  
run _ = liftIO $ printf "command not realizaded"

log :: Show a => a -> ST ()
log x = tell $ show x <> "\n"

create :: Context -> Name -> ST ()
create SimpleLayer name = do
    fixDirectory <- getFixDirectory
    s <- getLayersName
    liftIO $ createDirectoryIfMissing True $ fixDirectory </> setPrefix <> s </> "layers"
    addLayer name
    run (Command (Go DUp) "")
create SetLayers name = do
    run (Command Save "")
    fixDirectory <- getFixDirectory
    liftIO $ createDirectoryIfMissing True $ fixDirectory </> setPrefix <> name
    newLayers name

create _ _ = error "BUG: create"

getWorkState :: Route -> ST (Layer Body)
getWorkState [_] = (rBase . stLayers) <$> get
getWorkState _rt = undefined

getLayersName :: ST Name
getLayersName = (rName . stLayers) <$> get

makeLayerPath :: Path -> Route -> FilePath
makeLayerPath fixDir rt = fixDir </> head rt </> "layers" </> fun
  where
    fun = foldl1 (\x y -> x <> "." <> y) rt

getLayerName :: ST Name
getLayerName = (last . stPosition) <$> get

getLayers :: ST Layers
getLayers = stLayers <$> get

emptyLayers :: Layers
emptyLayers = Layers "base" "empty set of layers" (Layer ("base", [])) (Tree.Empty, [])

cleanLayers :: ST ()
cleanLayers = modify $ \s -> s { stLayers = emptyLayers }

newLayers :: Name -> ST () 
newLayers n = modify $ \s -> 
  s { stLayers = emptyLayers { rBase = emptyLayer n
                             , rName = n 
                             } 
    , stRoute = Map.empty
    , stPosition = [n]
    }

addLayer :: Name -> ST ()
addLayer n = modify $ \s ->
  s { stPosition = n : (stPosition s)
    }

emptyLayer :: Name -> Layer Body
emptyLayer n = Layer (n,[(".",D)])

{--
doesExists :: Context -> Layername -> ST Bool
doesExists OLayer layerName = liftIO . doesDirectoryExist =<< prefix OLayer layerName
doesExists _ _ = error $ "doesExists: not implemented"

--}
getFixDirectory :: ST Path
getFixDirectory = stFixDirectory <$> get

getWorkDirectory :: ST Path
getWorkDirectory = dropFileName . dropTrailingPathSeparator . stFixDirectory <$> get

updateState :: Options -> ST ()
updateState opts = modify $ 
    \s -> s { stVerbosity = optVerbosity opts }

{--
prefix :: Context -> Name -> ST Path
prefix OLayer layerName = (</> layerPrefix <> layerName) <$> getFixDirectory 
prefix Work _ = getWorkDirectory
prefix _ _ = error "prefix"

clean :: Context -> Name -> ST ()
clean c n = liftIO . cleanDirectory =<< prefix c n
cleanDirectory :: Path -> IO ()
cleanDirectory filePath = do
    cont <- getDirectoryContents filePath
    forM_ (dotFilter cont) $ \x -> rm (filePath </> x) 
    where
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

--}
dotFilter :: [Path] -> [Path]
dotFilter = filter (`notElem` [".", "..", ".fix"])

copyDirectory :: Path -> Path -> IO ()
copyDirectory fromDir toDir = do
    cont <- getDirectoryContents fromDir
    forM_ (dotFilter cont) $ \x -> cp (fromDir </> x) (toDir </> x)
    where
      cp :: Path -> Path -> IO ()
      cp f t = do
          temp <- try (copyFile f t)
          case temp of
               Right _ -> return ()
               Left e -> do
                   isDir <- doesDirectoryExist f
                   unless isDir $ throw (e :: SomeException)
                   createDirectory t
                   copyDirectory f t

    
{--
clone :: Context -> Context -> String -> String -> ST ()
clone OLayer Work layerName _ = do
    fromContext <- prefix OLayer layerName
    toContext <- prefix Work ""
    liftIO $ copyDirectory fromContext toContext
clone Work OLayer _ layerName = do
    fromContext <- prefix Work ""
    toContext <- prefix OLayer layerName
    liftIO $ copyDirectory fromContext toContext
    --}


-- clone _ _ _ _ = undefined

type ST = WriterT String (StateT Fix IO)

runST :: ST () -> Fix -> IO (String, Fix)
runST = runStateT . execWriterT

data Fix = 
  Fix { stFixDirectory    :: Path
      , stLayers          :: Layers
      , stRoute           :: Map Route (Layer Body)
      , stPosition        :: Route
      , stVerbosity       :: Verbosity
      } deriving (Eq, Generic)

instance Binary Fix

instance Show Fix where
    show x = "\nfixDirectory: " <> (show $ stFixDirectory x) 
           <> "\ncurrentLayer: " <> (show $ stLayers x) <> "\n"
           <> "\nposition: " <> (show $ stPosition x)
           -- <> "history: \n" <> (unlines $ map show (take 10 $ stHistory x))
