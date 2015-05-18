{-# LANGUAGE DeriveGeneric #-}
module Data.Types where

import           Data.Binary          (Binary)

import           Control.Monad.State
import           Control.Monad.Writer hiding (First)
import           Data.Layer
import           Data.Tree
import           GHC.Generics         (Generic)
import           Opts.Opts


type ST = WriterT String (StateT Fix IO)

runST :: ST () -> Fix -> IO (String, Fix)
runST = runStateT . execWriterT

data Fix =
  Fix { stFixDirectory :: Path
      , stBucket       :: Bucket
      , stVerbosity    :: Verbosity
      } deriving (Eq, Generic)

instance Binary Fix

instance Show Fix where
    show x = "\nfix directory: " <> (show $ stFixDirectory x)
           <> "\nbucket: " <> (rName . stBucket $ x ) <> "\n"
           <> "way: " <> (routeToString . route . rTree . stBucket $ x ) <> "\n"
           <> (show $ (rTree . stBucket) x) <> "\n"

routeToString :: Route -> String
routeToString [] = "-"
routeToString xs = foldl1 (\a b -> a <> "." <> b) xs


