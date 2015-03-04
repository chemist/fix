{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Directory
import System.FilePath
import Control.Monad.State
import Data.Binary (decodeFile, encodeFile, Binary)
import GHC.Generics (Generic)
import Text.Printf
import Data.Monoid ((<>))
import Control.Applicative

import Opts.Opts


path :: FilePath
path = "/Users/chemist/Develop/fix/tmp/"

fixDirectoryName :: Path
fixDirectoryName = ".fix"

fixFile :: Path
fixFile = "®««««««"

layerPrefix :: Path
layerPrefix = "®"

readState :: Path -> IO Fix
readState fixDirectory = do
    let fixState = fixDirectory </> fixFile
    stateAvailable <- doesFileExist fixState
    if stateAvailable
       then decodeFile fixState
       else return $ Fix fixDirectory Nothing Nothing [] Normal

writeState :: Path -> Fix -> IO ()
writeState fixDirectory = encodeFile $ fixDirectory </> fixFile

main :: IO ()
main = do
    command <-  parseOptions 
    let fixDirectory = optFixPath command </> fixDirectoryName
    fixDirectoryAvailable <-  doesDirectoryExist $ fixDirectory
    unless fixDirectoryAvailable $ error "Cant found fix directory, try fix init, or fix -f path to fix directory"
    writeState fixDirectory =<< execStateT (execute command) =<< readState fixDirectory
--     createDirectoryIfMissing True realp

execute :: Options -> ST ()
execute opts = do
    updateState opts
    run (optCommand opts)
    logState (optVerbosity opts)
    
run :: Command -> ST ()
run (Command Add Layer layerName) = do
    createIfMissing Layer layerName
    run (Command Switch Layer layerName)
run (Command Switch Layer layerName) = do
    isLayer <- doesExists Layer layerName
    if isLayer
       then do
           -- switch to layer code
           liftIO $ printf "end"
       else liftIO $ printf "layer does not exist, try create new layer"
    

run _ = liftIO $ printf "command not realizaded"

doesExists :: Subject -> Layername -> ST Bool
doesExists Layer layerName = liftIO . doesDirectoryExist =<< prefix Layer layerName
doesExists _ _ = error $ "doesExists: not implemented"

createIfMissing :: Subject -> Layername -> ST ()
createIfMissing Layer layerName = do
    fixDirectory <- getFixDirectory
    liftIO $ createDirectoryIfMissing True $ fixDirectory </> layerPrefix <> layerName
createIfMissing _ _ = liftIO . printf $ "createIfMissing: not implemented"

getFixDirectory :: ST Path
getFixDirectory = stFixDirectory <$> get

updateState :: Options -> ST ()
updateState opts = modify $ 
    \s -> s { stLastCommand = Just $ optCommand opts 
           , stHistory = optCommand opts : take 100 (stHistory s)
           , stVerbosity = optVerbosity opts
           }

logState :: Verbosity -> ST ()
logState Normal = return ()
logState Verbose = liftIO . printf =<< show <$> get


prefix :: Subject -> Name -> ST Path
prefix Layer layerName = (</> layerPrefix <> layerName) <$> getFixDirectory 
prefix _ _ = error "prefix"

type Name = String

type ST = StateT Fix IO

data Fix = 
  Fix { stFixDirectory :: Path
      , stCurrentLayer :: Maybe Path
      , stLastCommand  :: Maybe Command
      , stHistory      :: [Command]
      , stVerbosity   :: Verbosity
      } deriving (Eq, Generic)

instance Binary Fix

instance Show Fix where
    show x = "command: " <> (show $ stLastCommand x) <> "\n"
           <> "history: \n" <> (unlines $ map show (take 10 $ stHistory x))
