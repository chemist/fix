{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Directory
import System.FilePath
import System.Environment
import Control.Monad.State
import Control.Monad.Writer hiding (First)
import Data.Binary (decodeFile, encodeFile)
import qualified Data.ByteString.Lazy as BL
import Text.Printf
import Control.Applicative
import qualified Network.SSH.Client.LibSSH2 as SSH
import qualified Network.SSH.Client.LibSSH2.Foreign as SSH

import Prelude hiding (log)
import Helpers
import Command
import Types hiding (goUp)

import Opts.Opts

readState :: FilePath -> IO Fix
readState fixDirectory = do
    let fixState = fixDirectory </> fixStateName
    stateAvailable <- doesFileExist fixState
    if stateAvailable
       then setFixDirectory fixDirectory <$> decodeFile fixState
       else return $ setFixDirectory fixDirectory emptyFix

setFixDirectory :: FilePath -> Fix -> Fix
setFixDirectory filePath f = f { stFixDirectory = filePath }

writeState :: FilePath -> (String, Fix) -> IO ()
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

runInit :: FilePath -> Command -> IO ()
runInit fixPath' (Command Init _) = do
    createDirectoryIfMissing False $ fixPath' </> fixDirectoryName
    createDirectoryIfMissing False $ fixPath' </> fixDirectoryName </> "base"
    createDirectoryIfMissing False $ fixPath' </> fixDirectoryName </> "base" </> "layers"
runInit _ _ = error "Cant found fix directory, try fix init, or fix -f path to fix directory"

    
run :: Command -> ST ()
run (Command (Add LayerContext) name) = 
    ifM isEmpty
      (createLayer name >> (getRoute >>= \r -> goRoute (r <> [name])))
      (createLayer name)
run (Command BucketOpt name) = viewOrCreateOrSwitchBucket name
run (Command Init _) = return ()
run (Command Save _) = saveWorkSpaceAsLayer
run (Command (Go (ByRoute route')) _) = whenClean (goRoute route' >> cleanWorkSpace >> restoreWorkSpaceFromBucket) "you must save work directory"
run (Command (Go DUp) _) = whenClean goUp  "you must save work directory"
run (Command (Go DDown) _) = whenClean goDown  "you must save work directory"
run (Command DiffAction _) =
    ifM isWorkDirectoryClean
        showDiff
        (return ())
run (Command View _) = view

run _ = liftIO $ printf "command not realizaded"

runCommand :: String -> String -> Int -> String -> IO (Int, BL.ByteString)
runCommand login host port command =
      ssh login host port $ \s -> do
        SSH.withChannel s $ \ch -> do
           SSH.channelExecute ch command
           SSH.readAllChannel ch
           
runCommands :: String -> String -> Int -> [String] -> IO [(Int, BL.ByteString)]
runCommands login host port command =
    ssh login host port $ \s -> forM command (work s)
    where
      work :: SSH.Session -> String -> IO (Int, BL.ByteString)
      work session c = SSH.withChannel session $ \ch -> do
          SSH.channelExecute ch c
          SSH.readAllChannel ch

ssh :: String -> String -> Int -> (SSH.Session -> IO a) -> IO a
ssh login host port actions = do
      SSH.initialize True
      home <- getEnv "HOME"
      let known_hosts = home </> ".ssh" </> "known_hosts"
          public = home </> ".ssh" </> "id_rsa.pub"
          private = home </> ".ssh" </> "id_rsa"
      r <- SSH.withSSH2 known_hosts public private "" login host port $ actions
      SSH.exit
      return r
