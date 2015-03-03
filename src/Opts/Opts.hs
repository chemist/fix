{-# LANGUAGE DeriveGeneric #-}
module Opts.Opts where

import Options.Applicative
import Data.Map
import Data.Binary
import GHC.Generics (Generic)


parseOptions :: IO Options
parseOptions = execParser (info (Options <$> parseCommand <*> verbosity) idm)

parseCommand :: Parser Command
parseCommand = subparser
  (  command "add" (info (Command Add <$> subjects)
      ( progDesc "add" ))
  <> command "switch" (info (Command Switch <$> subjects)
      ( progDesc "switch" ))
  <> command "delete" (info (Command Delete <$> subjects)
      ( progDesc "delete" ))
  <> command "view" (info (Command Delete <$> subjects)
      ( progDesc "view" ))
  )

subjects :: Parser Subject
subjects = subparser
  (  command "host"    (info (Host <$> sm "HOSTNAME")
      ( progDesc "host"))
  <> command "layer"    (info (Layer <$> sm "LAYER")
      ( progDesc "layer"))
  <> command "user"    (info (User <$> sm "USER NAME")
      ( progDesc "user"))
  <> command "service" (info (Service <$> sm "SERVICE")
      ( progDesc "service"))
  <> command "check"   (info (Check <$> sm "PATH")
      ( progDesc "check"))
  <> command "template" (info (Template <$> sm "TEMPLATE")
      ( progDesc "template"))
  )
  where
    sm = strArgument . metavar

data Verbosity = Normal | Verbose deriving (Show, Eq, Generic)

instance Binary Verbosity

verbosity :: Parser Verbosity
verbosity = flag Normal Verbose
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose mode" )

data Options = Options
  { optCommand :: Command
  , optVerbosity :: Verbosity
  } deriving (Show, Eq, Generic)

instance Binary Options

data Command = Command Action Subject
  deriving (Show, Eq, Generic)

instance Binary Command

data Action = Add | Switch | Delete | View deriving (Show, Eq, Generic)

instance Binary Action 

data Subject
  = Host Hostname
  | Layer Layername 
  | User String
  | Service String
  | Check Path
  | Template Path
  deriving (Show, Eq, Generic)

instance Binary Subject

type Path = String
type Hostname = String
type Layername = String
type Weight = Int
type Env = Map String String

