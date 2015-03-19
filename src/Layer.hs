{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Layer where

import Data.Binary 
import System.Directory.Tree
import GHC.Generics (Generic)
import Data.ByteString (ByteString, readFile, writeFile)
import Data.ByteString.Lazy (toStrict, fromStrict)
import System.Posix.Types (FileMode, UserID, GroupID)
import System.FilePath
import Control.Applicative
import System.Posix.Files
import Crypto.Hash.MD5
import Data.Monoid ((<>))
import Control.Monad
import Data.Algorithm.Diff 
import Data.List (delete)
import GHC.IO.Exception
import Prelude hiding (readFile, writeFile)

-- import Opts.Opts
import Tree

instance Contexted CLayer where
    index (CLayer _ n) = n

type Comment = String

data CLayer = CLayer Comment Name deriving (Show, Eq, Generic)

instance Binary CLayer

data Layers = Layers
  { rName    :: Name
  , rComment :: String
  , rBase    :: Layer Body
  , rTree    :: Zipper Tree CLayer
  } deriving (Eq, Show, Generic)


instance Binary Layers

data Body = Body
  { bMD5   :: ByteString
  , bBody  :: ByteString
  , bMode  :: FileMode
  , bOwner :: UserID
  , bGroup :: GroupID
  } deriving (Eq, Generic)

instance Show Body where
    show _ = ""

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

newtype DTree a = DTree (AnchoredDirTree a) deriving (Eq, Generic, Binary)

instance Show a => Show (DTree a) where
    show (DTree (anc :/ f)) = anc ++ tail (unlines $ draw f)
      where
        draw (Dir n xs) = n : drawSubtree xs
        draw (File n x) = [n <> " " <> show x]
        draw (Failed{}) = ["failed"]

        drawSubtree [] = []
        drawSubtree [x] =    "|" : shift "`- " "   " (draw x)
        drawSubtree (x:ys) = "|" : shift "+- " "|  " (draw x) ++ drawSubtree ys

        shift first other = zipWith (++) (first : repeat other)

data DF a = D | F a deriving (Show, Eq, Generic)

instance Binary a => Binary (DF a)

deriving instance Generic (DirTree a)
deriving instance Generic (AnchoredDirTree a)

instance Binary IOException where
    put = error "BUG: can't put IOException"
    get = error "BUG: can't get IOException"

instance Binary a => Binary (DirTree a)
instance Binary a => Binary (AnchoredDirTree a)

class Loadable a where
    load :: Name -> FilePath -> IO a
    dump    :: FilePath -> a -> IO ()

instance Saveable a => Loadable (DTree a) where
    dump fp (DTree adt) = do
        r <- writeDirectoryWith save (fp :/ dirTree adt)
        when (anyFailed $ dirTree r) $ print $ "Error: " <> show (failures $ dirTree r)
    load n fp = do
        _ :/ t <- readDirectoryWith restore (addTrailingPathSeparator (normalise fp) <> ".")
        return $ DTree $ n :/ filterDir fun t
      where
      fun (Dir ".fix" _) = False
      fun Failed{} = False
      fun _ = True

instance (Saveable a, Show a, Eq a) => Loadable (Layer a) where
    dump fp l = dump fp (fromLayer l)
    load n fp = toLayer <$> load n fp


class Saveable a where
    restore :: FilePath -> IO a
    save :: FilePath -> a -> IO ()

instance Saveable Body where
    restore f = do
        bs <- readFile f
        fs <- getFileStatus f
        return $ Body (hash bs) bs (fileMode fs) (fileOwner fs) (fileGroup fs)
    save f (Body _ bs _ _ _) = do
        writeFile f bs

instance (Binary a, Saveable a) => Saveable (Layer a) where
    restore fp = decode . fromStrict <$> readFile fp
    save fp l = writeFile fp (toStrict . encode $ l)

instance (Binary a, Saveable a) => Saveable (Changes a) where
    save fp c = writeFile fp (toStrict . encode $ c)
    restore fp = decode . fromStrict <$> readFile fp

toLayer :: (Show a, Eq a) => DTree a -> Layer a
toLayer (DTree (a :/ dt)) = Layer (a, map (\(p, f) -> (joinPath $ reverse p, f)) $ toList' ([], dt))
  where
    toList' :: (Show a, Eq a) => ([FilePath], DirTree a) -> [([FilePath],DF a)]
    toList' (p, Dir n []) = [(n:p, D)]
    toList' (p, Dir n xs) = (n:p, D) : concatMap (\x -> toList' (n:p, x)) xs 
    toList' (p, File n f) = [(n:p, F f)]
    toList' e = error (show e)

fromLayer :: (Show a, Eq a) => Layer a -> DTree a
fromLayer (Layer (a, [])) = DTree $ a :/ Dir "." []
fromLayer (Layer (a, sx)) = DTree $ a :/ (foldl1 insert . map singleton $ sx)
  where
    singleton :: (Show a, Eq a) => (FilePath, DF a) -> DirTree a
    singleton (fp, df) = singleton' (splitPath fp, df)

    singleton' ([], _) = error "bug here"
    singleton' ([n], D) = Dir (dropTrailingPathSeparator n) []
    singleton' ([n], F x) = File (dropTrailingPathSeparator n) x
    singleton' (n:xs, x) = Dir (dropTrailingPathSeparator n) [singleton' (xs, x)]

    insert :: (Show a, Eq a) => DirTree a -> DirTree a -> DirTree a
    insert (Dir x xs) f@File{} = Dir x $ f:xs
    insert (Dir x []) (Dir _ xs) = Dir x xs
    insert (Dir x xs) (Dir _ []) = Dir x xs
    insert (Dir x xs) (Dir _ [y]) = Dir x $ insert' xs y
    insert _ _ = error "bad insert"

    insert' :: (Show a, Eq a) => [DirTree a] -> DirTree a -> [DirTree a]
    insert' [] n = [n]
    insert' (l:ls) n 
      | name l == name n = insert l n : ls
      | otherwise = l : insert' ls n

test :: (Show a, Eq a) => DTree a -> Bool
test x = x == (fromLayer . toLayer $ x)

newtype Changes a = Changes [Diff (FilePath, DF a)] deriving (Show, Generic, Eq)

newtype Layer a = Layer (FilePath, [(FilePath, DF a)]) deriving (Show, Generic, Eq)

deriving instance Generic (Diff a)
instance Binary a => Binary (Diff a)
instance Binary a => Binary (Changes a)
instance Binary a => Binary (Layer a)

getPatch :: (Show a, Eq a) => Layer a -> Layer a -> Changes a
getPatch (Layer (_, old)) (Layer (_, new)) = Changes (filter isDifference $ getDiff old new)
  where
    isDifference Both{} = False
    isDifference _ = True

data PatchType = Apply | Undo deriving (Eq, Show)

patch :: (Show a, Eq a) => PatchType -> Layer a -> Changes a -> Layer a
patch pt (Layer (anc, xs)) (Changes cs) = Layer (anc, patched)
  where
    patched = foldl change xs cs
    change ls (First x)  = if pt == Apply then delete x ls else x : ls
    change ls (Second x) = if pt == Undo  then delete x ls else x : ls
    change _ _ = error "Both can't be in Changes"

testPatch :: (Show a, Eq a) => Layer a -> Changes a -> Bool
testPatch l p = l == patch Undo (patch Apply l p) p



