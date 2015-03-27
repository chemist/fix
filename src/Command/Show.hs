module Command.Show where

import Control.Monad.State
import Control.Monad.Writer hiding (First)
import Text.Printf
import Control.Applicative
import Data.Maybe

import Command.Bucket
import Data.Layer 
import Data.Types
import qualified Data.Tree as Tree
import Helpers

import Prelude hiding (log)

view :: ST ()
view = do
    liftIO $ printf   "----- Available buckets ----- \n"
    viewOrCreateOrSwitchBucket ""
    liftIO $ printf $ "----------------------------- \n"
    (t, _) <- Tree.top . rTree <$> getBucket
    when (t /= Tree.Empty) $ do
      liftIO $ printf $ "----- Layers tree: ---------- \n" <> show t
      route <- routeToString . Tree.route . rTree <$> getBucket
      when (route /= "") $ do
        liftIO $ printf $ "----------------------------- \n"
        liftIO $ printf $ "Active layer: " <> route <> "\n"
        liftIO $ printf $ "----------------------------- \n"
        dTree <- fromLayer <$> getAllLayersFromBucket 
        liftIO $ printf $ "----- Filesystem tree: ------ \n"
        liftIO $ printf $ show dTree
    v <- Tree.value . rTree <$> getBucket
    maybe (return ()) diffs v
      where
        diffs v = do
          (r, a, ra) <- diffShow <$> loadChange v
          liftIO $ printf $ "----- Current layer: -------- \n"
          msg r
          msg a
          msg ra

showDiff :: ST ()
showDiff = do
    n <- getLayerName
    wd <- getWorkDirectory
    old <- getAllLayersFromBucket
    new <- liftIO $ load n wd :: ST (Layer Body)
    let changes = getPatch old new
    let (r, a, ra) = diffShow changes
    msg "Diff result..."
    msg r
    msg a
    msg ra


