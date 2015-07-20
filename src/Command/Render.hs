{-# LANGUAGE OverloadedStrings #-}
module Command.Render where

import Types
import Helpers
import System.Directory.Tree
import Control.Monad.State
import           Data.Yaml
import Data.DataFile.Environment
import           System.FilePath
import Data.Text (Text, pack)
import Data.Monoid 
import Data.HashMap.Strict hiding (foldr, map)


render :: ST ()
render = flip whenClean ("can't render workspace, it's dirty" :: String) $ do
    -- cleanWorkSpace
    fp <- getWorkDirectory
    DTree adt <- fromLayer <$> getAllLayersFromBucket
    let env = splitEnv $ zipPaths ("" :/ dirTree adt)
    result <- liftIO $ writeDirectoryWith (renderAll env) (fp :/ dirTree adt)
    liftIO $ print result
    liftIO $ print env

renderAll :: Value -> FilePath -> DF -> IO ()
renderAll _env fp a = print fp >> print a 


splitEnv :: DirTree (FilePath, DF) -> Value
splitEnv = Object . fromList . foldr splitValue []
  where
    splitValue :: (FilePath, DF) -> [(Text, Value)] -> [(Text, Value)]
    splitValue (fp, EN _ (Env (Object v) _)) xs =
        let l = map (\(key, val) -> (filePathToKey fp <> "." <> key, val)) $ toList v 
        in l <>  xs
    splitValue (fp, EN _ (Env x _)) xs  = (filePathToKey fp, x) : xs
    splitValue _ xs = xs

filePathToKey :: FilePath -> Text
filePathToKey ('.':'/':fp) = pack (map replaceToPoint $ dropExtension fp)
filePathToKey fp = error ("filePathToKey" ++ fp)

replaceToPoint :: Char -> Char
replaceToPoint '/' = '.'
replaceToPoint x = x
