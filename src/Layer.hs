{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Layer where

import Data.Binary 
import System.Directory.Tree
import GHC.Generics (Generic)
import Data.ByteString (ByteString, readFile, writeFile)
import System.Posix.Types (FileMode, UserID, GroupID)
import System.FilePath
import Control.Applicative
import System.Posix.Files
import Crypto.Hash.MD5
import Data.Monoid ((<>))
import Control.Monad
import Prelude hiding (readFile, writeFile)

import Opts.Opts

data FixLayer = FixLayer
  { lName :: String
  , lPath :: Path
  , lTree :: DirTree Body
  } deriving (Eq, Show, Generic)

data Body = Body
  { bMD5   :: ByteString
  , bBody  :: ByteString
  , bMode  :: FileMode
  , bOwner :: UserID
  , bGroup :: GroupID
  } deriving (Eq, Show, Generic)

instance Binary UserID where
    put = put . fromEnum
    get = toEnum <$> get

instance Binary GroupID where
    put = put . fromEnum 
    get = toEnum <$> get

instance Binary FileMode where
    put = put . fromEnum
    get = toEnum <$> get

instance Binary Body
instance Binary a => Binary (DirTree a) where
    put (File n a) = putWord8 0 >> put n >> put a
    put (Dir n t) = putWord8 1 >> put n >> put t
    put (Failed n e) = error $ "cant put file: " ++ show n ++ " error: " ++ show e
    get = unpackTree =<< getWord8
      where
      unpackTree 0 = File <$> get <*> get
      unpackTree 1 = Dir <$> get <*> get
      unpackTree _ = error "bad tag"

instance Binary a => Binary (AnchoredDirTree a) where
    put ((:/) f t) = put f >> put t
    get = (:/) <$> get <*> get

readBody :: FilePath -> IO Body
readBody f = do
    print f
    bs <- readFile f
    fs <- getFileStatus f
    return $ Body (hash bs) bs (fileMode fs) (fileOwner fs) (fileGroup fs)

writeBody :: FilePath -> Body -> IO ()
writeBody f (Body _ bs _ _ _) = do
    writeFile f bs

writeFix :: FilePath -> AnchoredDirTree Body -> IO ()
writeFix fp adt = do
    r <- writeDirectoryWith writeBody (fp :/ dirTree adt)
    when (anyFailed $ dirTree r) $ print $ "Error: " <> show (failures $ dirTree r)



readFix :: FilePath -> IO (AnchoredDirTree Body)
readFix fp = do
    p :/ t <- readDirectoryWith readBody (addTrailingPathSeparator (normalise fp) <> ".")
    return $ p :/ filterDir fun t
  where
  fun (Dir ".fix" _) = False
  fun _ = True



