{-# LANGUAGE DeriveGeneric #-}
module Opts.Opts where

import           Data.Binary
import           Data.Map            hiding (null)
import           Data.Tree
import           GHC.Generics        (Generic)
import           Options.Applicative

{--
fix
  показать рабочий стэк слоев с файлами
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
fix diff
fix save
  сохранить текущий слой
fix clean
  очистить рабочую директорию без сохранения
fix destroy $name
  физически удалить слой
--}

parseOptions :: IO Options
parseOptions =
  customExecParser (prefs showHelpOnError)
    (info (helper <*> (Options <$> parseCommand <*> verbosity <*> fixPath)) description)
  where
    description =
      (  fullDesc
      <> header "fix tool, util for manage your infrastructure."
      <> progDesc "Do it simple."
      )

parseCommand :: Parser Command
parseCommand = subparser
  (  command "add" (info (helper <*> (Command <$> (pure $ Add LayerContext) <*> sm "NAME"))
      ( progDesc "create new layer, and switch to him" ) )
  <> command "save" (info (Command Save <$> pure "")
      ( progDesc "save current layer" ))
  <> command "diff" (info (Command DiffAction <$> pure "")
      ( progDesc "show diff between workspace and active layer" ))
  <> command "bucket" (info (helper <*> (Command BucketOpt <$> sm "NAME"))
      ( progDesc "create new or switch to exist bucket" ))
  <> command "buckets" (info (Command BucketOpt <$> pure "")
      ( progDesc "show list of buckets" ))
  <> command "init" (info (Command Init <$> pure "")
      ( progDesc "initialize fix space" ))
  <> command "show" (info (Command View <$> pure "")
      ( progDesc "show current bucket" ))
  <> command "go" (info ( helper <*> (Command <$> (Go <$> parseDirection) <*> pure ""))
      ( progDesc "switch to layer"))
  <> command "help" (info ( helper <*> (Command <$> (Help <$> parseHelp) <*> pure ""))
      ( progDesc "show help"))
  )
  where
    sm = strArgument . metavar



parseRoute :: Parser Command
parseRoute = (Command <$> (Go <$> (ByRoute <$> (routeFromString <$> sm "ROUTE"))) <*> pure "")
  where
    sm = strArgument . metavar

parseContext :: Parser Context
parseContext = subparser ( command "bucket" (info (pure BucketContext) (progDesc "bucket")))

parseDirection :: Parser Direction
parseDirection = subparser
  (  command "up"    (info (pure DUp)       ( progDesc "up"))
  <> command "down"  (info (pure DDown)     ( progDesc "down"))
  <> command "right" (info (pure DLeft)     ( progDesc "right"))
  <> command "left"  (info (pure DRight)    ( progDesc "left"))
  )
  <|> (ByRoute <$> (routeFromString <$> sm))
  where
    sm = strArgument
      ( metavar "ROUTE"
      <> help "way in tree like: one.two.tree"
      )

parseHelp :: Parser HelpObject
parseHelp = subparser
  (  command "template"    (info (pure HTemplate)   ( progDesc "template"))
  <> command "access" (info (pure HAccessMode) ( progDesc "access mode"))
  <> command "env" (info (pure HEnv) ( progDesc "environment"))
  )

data HelpObject = HTemplate | HAccessMode | HEnv deriving (Show, Eq, Generic)

instance Binary HelpObject

routeFromString :: String -> Route
routeFromString t
  | null s'   = [l]
  | otherwise = l : routeFromString (tail s')
  where (l, s') = span (/= '.') t

data Verbosity = Normal | Verbose deriving (Show, Eq, Generic)

instance Binary Verbosity

fixPath :: Parser String
fixPath = strOption
  ( long "fix-path"
  <> short 'f'
  <> metavar "PATH"
  <> help "Path to workspace"
  ) <|> pure ""

verbosity :: Parser Verbosity
verbosity = flag Normal Verbose
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose mode" )

data Options = Options
  { optCommand   :: Command
  , optVerbosity :: Verbosity
  , optFixPath   :: Path
  } deriving (Show, Eq, Generic)

instance Binary Options

data Command = Command Action String
  deriving (Show, Eq, Generic)

instance Binary Command

data Direction = DUp | DDown | ByRoute Route | DLeft | DRight deriving (Show, Eq, Generic)
instance Binary Direction

data Context = BucketContext | LayerContext | WorkContext deriving (Show, Eq, Generic)
instance Binary Context

data Action = Add Context
            | Go Direction
            | BucketOpt
            | DiffAction
            | View
            | Init
            | Save
            | Help HelpObject
            deriving (Show, Eq, Generic)

instance Binary Action

type Path = String
type Hostname = String
type Weight = Int
type Env = Map String String

