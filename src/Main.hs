module Main where

-- import System.Directory
import System.FilePath
import Options.Applicative
import Data.Map


path :: FilePath
path = "/Users/chemist/Develop/fix/tmp/"

realp :: FilePath
realp = path </> ".fix"


main :: IO ()
main = do
    p <- execParser (info opts idm)
    print p
--     createDirectoryIfMissing True realp

opts :: Parser Command
opts = subparser
  (  command "add" (info (Command Add <$> addOptions)
      ( progDesc "add" ))
  <> command "switch" (info (Command Switch <$> switchOptions)
      ( progDesc "switch" ))
  )

addOptions :: Parser Entity
addOptions = subparser
  (  command "host"    (info (Host <$> sm "HOSTNAME")
      ( progDesc "host"))
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

switchOptions :: Parser Entity
switchOptions = subparser
  (  command "host"    (info (Host <$> sm "HOSTNAME")
      ( progDesc "host"))
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

data Verbosity = Normal | Verbose deriving (Show, Eq)

verbosity :: Parser Verbosity
verbosity = flag Normal Verbose
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose mode" )

data Options = Options
  { optCommand :: Command
  , optVerbosity :: Verbosity
  } deriving (Show, Eq)

data Command = Command Do Entity
  deriving (Show, Eq)

data Do = Add | Switch | Delete | View deriving (Show, Eq)

data Entity
  = Host Hostname
  | Layer Layername [(Weight, Entity, Env)]
  | User String
  | Service String
  | Check Path
  | Template Path
  deriving (Show, Eq)

type Path = String
type Hostname = String
type Layername = String
type Weight = Int
type Env = Map String String
