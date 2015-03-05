{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Directory
import System.FilePath
-- import System.Posix.Files
import Control.Monad.State
import Data.Binary (decodeFile, encodeFile, Binary)
import GHC.Generics (Generic)
import Text.Printf
import Data.Monoid ((<>))
import Control.Applicative
import Control.Exception.Base (try, throw, SomeException)

import Prelude hiding (log)

import Opts.Opts


path :: FilePath
path = "/Users/chemist/Develop/fix/tmp/"

fixDirectoryName :: Path
fixDirectoryName = ".fix"

fixFile :: Path
fixFile = "®««««««"

layerPrefix :: Path
layerPrefix = "®®"

emptyFix :: Fix
emptyFix = Fix "." Nothing Nothing [] Work Normal

readState :: Path -> IO Fix
readState fixDirectory = do
    let fixState = fixDirectory </> fixFile
    stateAvailable <- doesFileExist fixState
    if stateAvailable
       then setFixDirectory fixDirectory <$> decodeFile fixState
       else return $ setFixDirectory fixDirectory emptyFix

setFixDirectory :: Path -> Fix -> Fix
setFixDirectory filePath f = f { stFixDirectory = filePath }

writeState :: Path -> Fix -> IO ()
writeState fixDirectory = encodeFile $ fixDirectory </> fixFile

main :: IO ()
main = do
    command <-  parseOptions 
    let fixDirectory = optFixPath command </> fixDirectoryName
    fixDirectoryAvailable <-  doesDirectoryExist $ fixDirectory
    unless fixDirectoryAvailable $ runInit (optFixPath command) (optCommand command)
    writeState fixDirectory =<< execStateT (execute command) =<< readState fixDirectory
--     createDirectoryIfMissing True realp

execute :: Options -> ST ()
execute opts = do
    updateState opts
    run (optCommand opts)
    logState (optVerbosity opts)

runInit :: Path -> Command -> IO ()
runInit fixPath' (Command Init _ _) = do
    createDirectoryIfMissing False $ fixPath' </> fixDirectoryName
runInit _ _ = error "Cant found fix directory, try fix init, or fix -f path to fix directory"

    
run :: Command -> ST ()
run (Command Add Layer layerName) = do
    createIfMissing Layer layerName
    run (Command Switch Layer layerName)
run (Command Switch Layer layerName) = do
    isLayer <- doesExists Layer layerName
    if isLayer
       then do
           -- switch to layer code
           modify $ \s -> s { stCurrentLayer = Just layerName }
           clean Work ""
           clone Layer Work layerName ""
       else liftIO $ printf "layer does not exist, try create new layer"
run (Command Save _ _) = do
    Just layerName <- stCurrentLayer <$> get
    clean Layer layerName
    clone Work Layer "" layerName 
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
    


run _ = liftIO $ printf "command not realizaded"

log :: String -> StateT Fix IO ()
log x = liftIO . printf $ x <> "\n"

doesExists :: Context -> Layername -> ST Bool
doesExists Layer layerName = liftIO . doesDirectoryExist =<< prefix Layer layerName
doesExists _ _ = error $ "doesExists: not implemented"

createIfMissing :: Context -> Layername -> ST ()
createIfMissing Layer layerName = do
    fixDirectory <- getFixDirectory
    liftIO $ createDirectoryIfMissing True $ fixDirectory </> layerPrefix <> layerName
createIfMissing _ _ = liftIO . printf $ "createIfMissing: not implemented"

getFixDirectory :: ST Path
getFixDirectory = stFixDirectory <$> get

getWorkDirectory :: ST Path
getWorkDirectory = dropFileName . dropTrailingPathSeparator . stFixDirectory <$> get

updateState :: Options -> ST ()
updateState opts = modify $ 
    \s -> s { stLastCommand = Just $ optCommand opts 
           , stHistory = optCommand opts : take 100 (stHistory s)
           , stVerbosity = optVerbosity opts
           }

logState :: Verbosity -> ST ()
logState Normal = return ()
logState Verbose = liftIO . printf =<< show <$> get


prefix :: Context -> Name -> ST Path
prefix Layer layerName = (</> layerPrefix <> layerName) <$> getFixDirectory 
prefix Work _ = getWorkDirectory
prefix _ _ = error "prefix"

dotFilter :: [Path] -> [Path]
dotFilter = filter (`notElem` [".", "..", ".fix"])

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

    
clone :: Context -> Context -> String -> String -> ST ()
clone Layer Work layerName _ = do
    fromContext <- prefix Layer layerName
    toContext <- prefix Work ""
    liftIO $ copyDirectory fromContext toContext
clone Work Layer _ layerName = do
    fromContext <- prefix Work ""
    toContext <- prefix Layer layerName
    liftIO $ copyDirectory fromContext toContext


clone _ _ _ _ = undefined

type Name = String

type ST = StateT Fix IO

data Fix = 
  Fix { stFixDirectory :: Path
      , stCurrentLayer :: Maybe Path
      , stLastCommand  :: Maybe Command
      , stHistory      :: [Command]
      , stContext      :: Context
      , stVerbosity    :: Verbosity
      } deriving (Eq, Generic)

instance Binary Fix

instance Show Fix where
    show x = "\ncommand: " <> (show $ stLastCommand x)
           <> "\nfixDirectory: " <> (show $ stFixDirectory x) 
           <> "\ncurrentLayer: " <> (show $ stCurrentLayer x)
           -- <> "history: \n" <> (unlines $ map show (take 10 $ stHistory x))
