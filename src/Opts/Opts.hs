{-# LANGUAGE DeriveGeneric #-}
module Opts.Opts where

import Options.Applicative
import Data.Map
import Data.Binary
import GHC.Generics (Generic)


parseOptions :: IO Options
parseOptions = execParser (info (Options <$> parseCommand <*> verbosity <*> (fixPath)) idm)

parseCommand :: Parser Command
parseCommand = subparser
  (  command "add" (info (Command Add <$> subjects <*> sm "TEXT")
      ( progDesc "add" ))
  <> command "switch" (info (Command Switch <$> subjects <*> sm "TEXT")
      ( progDesc "switch" ))
  <> command "delete" (info (Command Delete <$> subjects <*> sm "TEXT")
      ( progDesc "delete" ))
  <> command "view" (info (Command Delete <$> subjects <*> sm "TEXT")
      ( progDesc "view" ))
  )
  where
    sm = strArgument . metavar

subjects :: Parser Subject
subjects = subparser
  (  command "host"    (info (pure Host)
      ( progDesc "host"))
  <> command "layer"    (info (pure Layer)
      ( progDesc "layer"))
  <> command "user"    (info (pure User)
      ( progDesc "user"))
  <> command "service" (info (pure Service)
      ( progDesc "service"))
  <> command "check"   (info (pure Check)
      ( progDesc "check"))
  <> command "template" (info (pure Template)
      ( progDesc "template"))
  )

data Verbosity = Normal | Verbose deriving (Show, Eq, Generic)

instance Binary Verbosity

fixPath :: Parser String
fixPath = strOption
  ( long "fix-path" <> short 'f' <> metavar "PATH" ) <|> pure "."

verbosity :: Parser Verbosity
verbosity = flag Normal Verbose
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose mode" )

data Options = Options
  { optCommand :: Command
  , optVerbosity :: Verbosity
  , optFixPath :: Path
  } deriving (Show, Eq, Generic)

instance Binary Options

data Command = Command Action Subject String
  deriving (Show, Eq, Generic)

instance Binary Command

data Action = Add | Switch | Delete | View deriving (Show, Eq, Generic)

instance Binary Action 

data Subject
  = Host
  | Layer
  | User
  | Service
  | Check
  | Template
  deriving (Show, Eq, Generic)

instance Binary Subject

type Path = String
type Hostname = String
type Layername = String
type Weight = Int
type Env = Map String String

