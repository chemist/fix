module Command.Go where

import System.Directory
import System.FilePath
import Control.Monad.State
import Control.Applicative
import Control.Exception.Base (try, SomeException)
import Data.Maybe
import Data.Algorithm.Diff 

import Opts.Opts
import Types hiding (goUp)
import qualified Types as T
import Helpers

import Prelude hiding (log)

goRoute :: Route -> ST ()
goRoute route' = do
    log "go route"
    bucket <- getBucket
    modify $ \s -> s { stBucket = bucket { rTree = (goClosest route' (rTree bucket)) }}

goUp :: ST ()
goUp = do
    log "go up"
    bucket <- getBucket
    case goLevel (rTree bucket) of
         Nothing -> return ()
         Just new -> do
             -- get value from new tree
             -- First -- remove
             -- Second -- add
             Changes changes <- loadChange $ fromJust $ value new
             patchWorkSpace DUp changes
             modify $ \s -> s { stBucket = bucket { rTree = new }}

goDown :: ST ()
goDown = do
    log "go down"
    bucket <- getBucket
    case T.goUp (rTree bucket) of
         Nothing -> return ()
         Just new -> do
             -- get value from old tree
             -- First - add
             -- Second -- remove
             old <- rTree <$> getBucket
             Changes changes <- loadChange $ fromJust (value old)
             patchWorkSpace DDown changes
             modify $ \s -> s { stBucket = bucket { rTree = new }}

patchWorkSpace :: Direction -> [Diff (FilePath, DF)] -> ST ()
patchWorkSpace d xs = do
    -- split to 2 line, remove first, restore second
    let (f, s) = (filter (fun d) xs, filter (not . (fun d)) xs)
        fun DUp First{} = True
        fun DDown Second{} = True
        fun _ _ = False
    patchWorkSpace' (length f + 1) d f
    patchWorkSpace' (length s + 1) d s
    where
      -- | recurse allowed only i times, where i -> length xs + 1
      patchWorkSpace' :: Int -> Direction -> [Diff (FilePath, DF )] -> ST ()
      patchWorkSpace' _ _ [] = return ()
      patchWorkSpace' 0 _ _ = error "problem when workWithFile"
      patchWorkSpace' i y ys = do
          let queue = map (workWithFile y) ys
          result <- sequence queue
          let new = map snd . filter (not . fst) $ zip result ys
          patchWorkSpace' (i - 1) y new

      workWithFile :: Direction -> Diff (FilePath, DF ) -> ST Bool
      workWithFile DDown (First (p, f)) = restoreFile p f
      workWithFile DDown (Second (p, f)) = rmFile p f
      workWithFile DUp   (First (p, f)) = rmFile p f
      workWithFile DUp   (Second (p, f)) = restoreFile p f
      workWithFile _ _ = error "not implemented in workWithFile"
      
      rmFile :: FilePath -> DF -> ST Bool
      rmFile f D = do
          wd <- getWorkDirectory
          e <- liftIO $ try $ removeDirectory (normalise $ wd </> f)
          return $ either (const False) (const True) (e :: Either SomeException ())
      rmFile f _ = do
          wd <- getWorkDirectory
          e <- liftIO $ try $ removeFile (normalise $ wd </> f)
          return $ either (const False) (const True) (e :: Either SomeException ())
      
      restoreFile :: FilePath -> DF  -> ST Bool
      restoreFile f D = do
          wd <- getWorkDirectory
          e <- liftIO $ try $ createDirectory (normalise $ wd </> f)
          return $ either (const False) (const True) (e :: Either SomeException ())
      restoreFile f b = do
          wd <- getWorkDirectory
          e <- liftIO $ try $ restore (normalise $ wd </> f) b 
          return $ either (const False) (const True) (e :: Either SomeException ())



