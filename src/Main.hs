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

realp :: FilePath
realp = path </> ".fix"

statePath :: FilePath
statePath = realp </> "®««««««"

readState :: IO Fix
readState = do
    stateAvailable <- doesFileExist statePath
    if stateAvailable
       then decodeFile statePath
       else return $ Fix Nothing Nothing [] Normal

writeState :: Fix -> IO ()
writeState = encodeFile statePath

main :: IO ()
main = do
    command <-  parseOptions 
    fixDirectoryAvailable <-  doesDirectoryExist realp
    unless fixDirectoryAvailable $ error "Cant found fix directory, try fix init, or go to right directory"
    writeState =<< execStateT (execute command) =<< readState
--     createDirectoryIfMissing True realp

execute :: Options -> ST ()
execute opts = do
    updateState opts
    logState (optVerbosity opts)
    

updateState :: Options -> ST ()
updateState opts = modify $ 
    \s -> s { stLastCommand = Just $ optCommand opts 
           , stHistory = optCommand opts : take 100 (stHistory s)
           , stVerbosity = optVerbosity opts
           }

logState :: Verbosity -> ST ()
logState Normal = return ()
logState Verbose = liftIO . printf =<< show <$> get



type ST = StateT Fix IO

data Fix = 
  Fix { stCurrentLayer :: Maybe Path
      , stLastCommand  :: Maybe Command
      , stHistory      :: [Command]
      , stVerbosity   :: Verbosity
      } deriving (Eq, Generic)

instance Binary Fix

instance Show Fix where
    show x = "command: " <> (show $ stLastCommand x) <> "\n"
           <> "history: \n" <> (unlines $ map show (take 10 $ stHistory x))
