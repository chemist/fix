{-# LANGUAGE DeriveGeneric #-}
module Opts.Opts where

import Options.Applicative
import Data.Map
import Data.Binary
import GHC.Generics (Generic)

{--
fix
  показать рабочий стэк слоев с файлами
fix show
  показать все слои с файлами
fix describe $name
  показать слой $name с файлами
fix add $name
  fix save
  добавить слой поверх текущего
  переключиться на него
fix remove $name
  удалить слой
fix remove
  fix remove верхний слой
  убрать файлы верхнего слоя из рабочей директории
fix up
  переключиться на слой выше если есть
fix down 
  переключиться на слой ниже если есть
fix swap
  переключиться между текущим и предыдущим слоем
fix swap $name
fix save
  сохранить текущий слой
fix clean
  очистить рабочую директорию без сохранения
  очистить стэк слоев
fix destroy $name
  физически удалить слой
fix save and clean
  fix save
  fix clean
--}

parseOptions :: IO Options
parseOptions = execParser (info (Options <$> parseCommand <*> verbosity <*> (fixPath)) idm)

parseCommand :: Parser Command
parseCommand = subparser
  (  command "add" (info (Command Add <$> subjects <*> sm "NAME")
      ( progDesc "add" ))
  <> command "switch" (info (Command Switch <$> subjects <*> sm "NAME")
      ( progDesc "switch" ))
  <> command "delete" (info (Command Delete <$> subjects <*> sm "NAME")
      ( progDesc "delete" ))
  <> command "view" (info ((Command View <$> subjects  <*> sm "NAME") <|> (Command View <$> pure Work <*> pure ""))
      ( progDesc "view" ))
  <> command "pwd" (info (Command Pwd <$> pure Work <*> pure "")
      ( progDesc "pwd" ))
  <> command "init" (info (Command Init <$> pure Work <*> pure "")
      ( progDesc "init" ))
  <> command "save" (info (Command Save <$> pure Work <*> pure "")
      ( progDesc "save" ))
  )
  where
    sm = strArgument . metavar

subjects :: Parser Context
subjects = subparser
  (  command "host"    (info (pure Host) 
      ( progDesc "host")) <> metavar "CONTEXT: host | layer | user | service | check | template"
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
  ( long "fix-path" <> short 'f' <> metavar "PATH" ) <|> pure ""

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

data Command = Command Action Context String
  deriving (Show, Eq, Generic)

instance Binary Command

data Action = Add 
            | Switch 
            | Delete 
            | View 
            | Pwd 
            | Init 
            | Save 
            deriving (Show, Eq, Generic)

instance Binary Action 

data Context
  = Host
  | Layer
  | User
  | Service
  | Check
  | Template
  | Work
  deriving (Show, Eq, Generic)

instance Binary Context

type Path = String
type Hostname = String
type Layername = String
type Weight = Int
type Env = Map String String

