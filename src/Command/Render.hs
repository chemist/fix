{-# LANGUAGE OverloadedStrings #-}
module Command.Render where

import           Command.Clean
import           Control.Monad.State
import qualified Data.ByteString           as B
import           Data.DataFile.Environment
import           Data.DataFile.Template    (template)
import           Data.Foldable             (foldrM)
import           Data.HashMap.Strict       hiding (foldr, map)
import           Data.IORef
import           Data.Text                 (Text, pack)
import qualified Data.Text.Lazy.IO         as T
import           Data.Yaml
import           Helpers
import           System.Directory.Tree
import           System.FilePath
import           Text.EDE
import           Types


-- TODO replace union
--
render :: ST ()
render = flip whenClean ("can't render workspace, it's dirty" :: String) $ do
    cleanWorkSpace
    fp <- getWorkDirectory
    DTree adt <- fromLayer <$> getAllLayersFromBucket
    let env = splitEnv $ zipPaths ("" :/ dirTree adt)
    scripts <- splitScripts $ zipPaths ("" :/ dirTree adt)
    liftIO $ print $ union env scripts
    st <- get
    renderError <- liftIO $ newIORef []
    _ <- liftIO $ writeDirectoryWith (\x y -> runST (renderAll renderError (union env scripts) x y) st >> return ()) (fp :/ dirTree adt)
    renderError' <- liftIO $ readIORef renderError
    if (renderError' == [])
       then put (st { stIsRender = True })
       else do
          clean
          msg ("can't render templates:" :: String)
          mapM_ (liftIO . putStrLn) renderError'

renderAll :: IORef [String] -> Object -> FilePath -> DF -> ST ()
renderAll _ _env fp (F _ bs) = do
    liftIO $ B.writeFile fp bs
renderAll renderError env fp (T _ tpl) = do
    let fn = dropExtension fp
        rendered = eitherRender (template tpl) env
    case rendered of
         Left error' -> liftIO $ modifyIORef' renderError $ \x -> ("file: " ++ goodFilePath ++ " error" ++ toError error') : x
         Right txt -> liftIO $ T.writeFile fn txt
    where
      ('.':'/':goodFilePath) = fp
      toError = dropWhile (/= ':') . tail . dropWhile (/=':') . tail . dropWhile (/=':')
renderAll _ _ _ _ = return ()

splitEnv :: DirTree (FilePath, DF) -> Object
splitEnv = foldr splitObject empty
  where
    splitObject :: (FilePath, DF) -> Object -> Object
    splitObject (fp, EN _ (Env obj _)) splitted =
        let single = filePathToObject fp obj
        in union splitted single
    splitObject _ x = x

splitScripts :: DirTree (FilePath, DF) -> ST Object
splitScripts = foldrM splitObject empty
  where
    splitObject :: (FilePath, DF) -> Object -> ST Object
    splitObject (fp, S _ _bs) splitted = do
        return $ union splitted $ filePathToObject fp Null
    splitObject _ x = return x

filePathToKey :: FilePath -> Text
filePathToKey ('.':'/':fp) = pack (map replaceToPoint $ dropExtension fp)
filePathToKey fp = error ("filePathToKey" ++ fp)

filePathToObject :: FilePath -> Value -> Object
filePathToObject ('.':'/':fp) o =
    let splitted = splitDirectories $ map replaceToSlash $ dropExtension fp
        Object obj = object $ foldr fun [(pack $ last splitted) .= o] (init splitted)
    in obj
    where
      fun key xs = [(pack key) .= object xs]
filePathToObject fp _ = error ("filePathToObject" ++ fp)

replaceToSlash :: Char -> Char
replaceToSlash '.' = '/'
replaceToSlash x = x

replaceToPoint :: Char -> Char
replaceToPoint '/' = '.'
replaceToPoint x = x
