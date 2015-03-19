{-# LANGUAGE DeriveGeneric #-}
module Opts.Opts where

import Options.Applicative
import Data.Map hiding (null)
import Data.Binary
import GHC.Generics (Generic)
import Tree

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
  fix up
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
  (  command "add" (info (Command <$> (Add <$> (parseContext <|> pure SimpleLayer)) <*> sm "NAME")
      ( progDesc "add" ))
  <> command "save" (info (Command Save <$> pure "")
      ( progDesc "save" ))
  <> command "init" (info (Command Init <$> pure "")
      ( progDesc "init" ))
  <> command "go" (info (Command <$> (Go <$> parseDirection) <*> pure "")
      ( progDesc "go up | down | left | right | way ..." ))
  ) <|> pure (Command View "")
  where
    sm = strArgument . metavar

parseContext :: Parser Context
parseContext = subparser ( command "set" (info (pure SetLayers) (progDesc "set"))) 

parseDirection :: Parser Direction
parseDirection = subparser
  (  command "up"    (info (pure DUp)       ( progDesc "up"))
  <> command "down"  (info (pure DDown)     ( progDesc "down"))
  <> command "right" (info (pure DLeft)     ( progDesc "right"))
  <> command "left"  (info (pure DRight) ( progDesc "left"))
  <> command "way"   (info (ByRoute <$> (routeFromString <$> sm "ROUTE")) ( progDesc "way"))
  )
  where
    sm = strArgument . metavar

routeFromString :: String -> Route
routeFromString t 
  | null s'   = [l]
  | otherwise = l : routeFromString (tail s')
  where (l, s') = span (/= '.') t

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

data Command = Command Action String
  deriving (Show, Eq, Generic)

instance Binary Command

data Direction = DUp | DDown | ByRoute Route | DLeft | DRight deriving (Show, Eq, Generic)

instance Binary Direction

data Context = SetLayers | SimpleLayer | Work deriving (Show, Eq, Generic)
instance Binary Context

data Action = Add Context
            | Go Direction
            | Delete 
            | View 
            | Pwd 
            | Init 
            | Save 
            deriving (Show, Eq, Generic)

instance Binary Action 

type Path = String
type Hostname = String
type Weight = Int
type Env = Map String String

