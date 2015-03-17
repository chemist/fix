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
import Data.Algorithm.Diff
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
instance Binary a => Binary (DirTree a) where
    put (File n a) = putWord8 0 >> put n >> put a
    put (Dir n t) = putWord8 1 >> put n >> put t
    put (Failed n e) = error $ "cant put file: " ++ show n ++ " error: " ++ show e
    get = unpackTree =<< getWord8
      where
      unpackTree 0 = File <$> get <*> get
      unpackTree 1 = Dir <$> get <*> get
      unpackTree _ = error "bad tag"

instance Binary a => Binary (DTree a) where
    put (DTree (f :/ t)) = put f >> put t
    get = DTree <$> ((:/) <$> get <*> get)

readBody :: FilePath -> IO Body
readBody f = do
    bs <- readFile f
    fs <- getFileStatus f
    return $ Body (hash bs) bs (fileMode fs) (fileOwner fs) (fileGroup fs)

writeBody :: FilePath -> Body -> IO ()
writeBody f (Body _ bs _ _ _) = do
    writeFile f bs

writeDTree :: FilePath -> DTree Body -> IO ()
writeDTree fp (DTree adt) = do
    r <- writeDirectoryWith writeBody (fp :/ dirTree adt)
    when (anyFailed $ dirTree r) $ print $ "Error: " <> show (failures $ dirTree r)



readDTree :: FilePath -> IO (DTree Body)
readDTree fp = do
    p :/ t <- readDirectoryWith readBody (addTrailingPathSeparator (normalise fp) <> ".")
    return $ DTree $ p :/ filterDir fun t
  where
  fun (Dir ".fix" _) = False
  fun _ = True

newtype DTree a = DTree (AnchoredDirTree a) deriving (Eq)

instance Show a => Show (DTree a) where
    show (DTree (path :/ f)) = path ++ tail (unlines $ draw f)
      where
        draw (Dir n xs) = n : drawSubtree xs
        draw (File n x) = [n <> " " <> show x]
        draw (Failed{}) = ["failed"]

        drawSubtree [] = []
        drawSubtree [x] =    "|" : shift "`- " "   " (draw x)
        drawSubtree (x:ys) = "|" : shift "+- " "|  " (draw x) ++ drawSubtree ys

        shift first other = zipWith (++) (first : repeat other)

data DF a = D | F a deriving (Show, Eq)

toList :: (Show a, Eq a) => DTree a -> (FilePath, [(FilePath, DF a)])
toList (DTree (a :/ dt)) = (a, map (\(p, f) -> (joinPath $ reverse p, f)) $ toList' ([], dt))
  where
    toList' :: (Show a, Eq a) => ([FilePath], DirTree a) -> [([Path],DF a)]
    toList' (p, Dir n []) = [(n:p, D)]
    toList' (p, Dir n xs) = (n:p, D) : concatMap (\x -> toList' (n:p, x)) xs 
    toList' (p, File n f) = [(n:p, F f)]
    toList' e = error (show e)

fromList :: (Show a, Eq a) => (FilePath, [(FilePath, DF a)]) -> DTree a
fromList (a, sx) = DTree $ a :/ (foldl1 insert . map singleton $ sx)
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
test x = x == (fromList . toList $ x)


onlySecond :: [Diff a] -> [a]
onlySecond [] = []
onlySecond (Second x : xs) = x : onlySecond xs
onlySecond (_: xs) = onlySecond xs

