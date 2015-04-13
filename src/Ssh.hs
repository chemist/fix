module Ssh where

import Network.SSH.Client.LibSSH2 
import Network.SSH.Client.LibSSH2.Foreign 
import Data.ByteString.Lazy hiding (pack)
import Data.ByteString.Char8 (pack)
import Control.Monad
import System.Environment
import System.FilePath


runCommand :: String -> String -> Int -> String -> IO (Int, ByteString)
runCommand login host port command =
      ssh login host port $ \s -> do
        withChannel s $ \ch -> do
           channelExecute ch command
           readAllChannel ch
           
runCommands :: String -> String -> Int -> [String] -> IO [(Int, ByteString)]
runCommands login host port command =
    ssh login host port $ \s -> forM command (work s)
    where
      work :: Session -> String -> IO (Int, ByteString)
      work session c = withChannel session $ \ch -> do
          channelExecute ch c
          readAllChannel ch

ssh :: String -> String -> Int -> (Session -> IO a) -> IO a
ssh login host port actions = do
      initialize True
      home <- getEnv "HOME"
      let known_hosts = home </> ".ssh" </> "known_hosts"
          public = home </> ".ssh" </> "id_rsa.pub"
          private = home </> ".ssh" </> "id_rsa"
      r <- withSSH2 known_hosts public private "" login host port $ actions
      exit
      return r

run :: [String] -> IO (Int, [ByteString])
run = ssh "chemist" "seven" 22 . flip runShellCommands

runShell :: Session -> [String] -> IO (Int, [ByteString])
runShell s commands = withChannel s $ \ch -> do
    requestPTY ch "linux"
    print "request"
    channelShell ch
    print "channel"
    _hello <- readAllChannel ch
    print _hello
    out <- forM commands $ \cmd -> do
        writeChannel ch (pack $ cmd ++ "\n")
        r <- readAllChannel ch
        return r
    channelSendEOF ch
    return out
