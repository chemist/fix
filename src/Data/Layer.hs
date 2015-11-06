{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Layer where

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash.MD5
import           Data.Algorithm.Diff
import           Data.Attoparsec.ByteString.Char8 (parseOnly)
import           Data.Binary
import           Data.ByteString                  (ByteString, readFile,
                                                   writeFile)
import           Data.List                        (delete, sortBy)
import           Data.Monoid                      ((<>))
import qualified Data.Set                         as Set
import           Data.Yaml                        (decodeEither)
import           GHC.Generics                     (Generic)
import           GHC.IO.Exception
import           Prelude                          hiding (readFile, writeFile)
import           System.Directory.Tree
import           System.FilePath
import qualified Text.EDE                         as E
-- import           Data.ByteString.Char8 (unpack)

import           Data.DataFile.AccessMode
import qualified Data.DataFile.AccessMode         as M
import           Data.DataFile.Environment
import qualified Data.DataFile.Environment        as Env
import           Data.DataFile.Template
import qualified Data.DataFile.Template           as T
import           Data.Tree

instance Contexted CLayer where
    index (CLayer _ n _) = n

instance Contexted (Layer DF) where
    index (Layer (n, _)) = n

type Comment = String

type HashName = String

data CLayer = CLayer Comment Name HashName deriving (Eq, Generic)

instance Show CLayer where
    show _ = ""

instance Binary CLayer

data Bucket = Bucket
  { rName    :: Name
  , rComment :: String
  , rBase    :: Layer DF
  , rTree    :: Zipper Tree CLayer
  } deriving (Eq, Show, Generic)

instance Binary Bucket

newtype DTree a = DTree (AnchoredDirTree a) deriving (Eq, Generic, Binary)

instance Show a => Show (DTree a) where
    show (DTree (anc :/ f)) = view
      where
        view :: String
        view =
            let tree = draw f
            in anc ++ "\n" ++ tail (unlines $ splitSpecial tree)
        draw :: Show a => DirTree a -> [String]
        draw (Dir n xs) = "/" <> n : drawSubtree xs
        draw (File n x)
          | n == "_fix_access_mode_" = [show x]
          | (snd $ splitExtension n) == ".ede" = ["/" <> (fst $ splitExtension n) <> show x]
          | (snd $ splitExtension n) == ".fix_mode" = ["/" <> (fst $ splitExtension n) <> show x]
          | (snd $ splitExtension n) == ".fix_env" = ["/" <> (fst $ splitExtension n) <> show x]
          | (snd $ splitExtension n) == ".fix_run" = ["/" <> (fst $ splitExtension n) <> show x]
          | otherwise = ["/" <> n <> show x]
        draw (Failed{}) = ["failed"]

        drawSubtree :: Show a => [DirTree a] -> [String]
        drawSubtree [] = []
        drawSubtree [x] = "|" : shift "`- " "   " (draw x)
        drawSubtree (x:ys) = "|" : shift "+- " "|  " (draw x) ++ (drawSubtree ys)

        shift first other = zipWith (++) (first : repeat other)
        splitSpecial ("|":"|":ys) = splitSpecial ("|":ys)
        splitSpecial ("|":y:ys) = "|" : splitSpecial (y:ys)
        splitSpecial (x:y:ys) = x : splitSpecial (y:ys)
        splitSpecial x = x

newtype MD5 = MD5 ByteString deriving (Show, Eq, Ord, Generic)

instance Binary MD5

data DF = D
        | M MD5 AccessMode
        | F MD5 ByteString
        | S MD5 ByteString
        | T MD5 Tpl
        | EN MD5 Env
        deriving (Eq, Ord, Generic)

instance Show DF where
    show D = ""
    show (F _ _bs)   = " :: File " -- ++ unpack bs
    show (S _ _bs)   = " :: Script " -- ++ unpack bs
    show (M _ _am)   = " :: AccessMode" -- show am
    show (T _ _tpl)  = " :: Template" -- show tpl
    show (EN _ _env) = " :: Env" -- show env

type PathRegexp = String

instance Binary DF

deriving instance Generic (DirTree a)
deriving instance Generic (AnchoredDirTree a)

instance Binary IOException where
    put = error "BUG: can't put IOException"
    get = error "BUG: can't get IOException"

instance Binary a => Binary (DirTree a)
instance Binary a => Binary (AnchoredDirTree a)

class Restorable a where
    -- restore from a to raw files
    restore :: FilePath -> a -> IO ()
    -- make a from raw files
    dump    :: Name -> FilePath -> IO a

instance Restorable (Layer DF) where
    restore fp l = restore fp (fromLayer l)
    dump n fp = toLayer <$> dump n fp

instance (Restorable a) => Restorable (DTree a) where
    restore fp (DTree adt) = do
        r <- writeDirectoryWith restore (fp :/ dirTree adt)
        when (anyFailed $ dirTree r) $ print $ "Error: " <> show (failures $ dirTree r)
    dump n fp = do
        _ :/ t <- readDirectoryWith (dump "") (addTrailingPathSeparator (normalise fp) <> ".")
        return $ DTree $ n :/ filterDir fun t
      where
      fun (Dir ".fix" _) = False
      fun Failed{} = False
      fun _ = True

instance Restorable DF where
    restore f (F _ bs) = writeFile f bs
    restore f (M _ bs) = writeFile f (M.rawFile bs)
    restore f (T _ bs) = writeFile f (T.rawFile bs)
    restore f (S _ bs) = writeFile f bs
    restore f (EN _ bs) = writeFile f (Env.rawFile bs)
    restore _ _ = undefined
    dump _ f = do
        bs <- readFile f
        case (snd $ splitExtension f) of
             ".fix_mode" -> return $ M (MD5 $ hash bs) (right (parseOnly (accessMode bs) bs))
             ".fix_env"  -> return $ EN (MD5 $ hash bs) (parseEnvironment bs)
             ".fix_run"  -> return $ S (MD5 $ hash bs) bs
             ".ede"      -> return $ T (MD5 $ hash bs) (parseTpl bs)
             _           -> return $ F (MD5 $ hash bs) bs
        where
          right (Right x) = x
          right _ = error "right: bad parse result"
          parseTpl bs = case E.parse bs of
                             E.Success tpl -> Tpl tpl bs
                             _ -> error "parseTpl: bad template"
          parseEnvironment :: ByteString -> Env
          parseEnvironment bs = case decodeEither bs of
                                     Right o -> Env o bs
                                     Left e -> error e

toLayer :: DTree DF -> Layer DF
toLayer (DTree (a :/ dt)) = Layer (a, map (\(p, f) -> (joinPath $ reverse p, f)) $ toList' ([], dt))
  where
    toList' :: ([FilePath], DirTree DF) -> [([FilePath],DF)]
    toList' (p, Dir n []) = [(n:p, D)]
    toList' (p, Dir n xs) = (n:p, D) : concatMap (\x -> toList' (n:p, x)) xs
    toList' (p, File n f) = [(n:p, f)]
    toList' e = error (show e)

fromLayer :: Layer DF -> DTree DF
fromLayer (Layer (a, [])) = DTree $ a :/ Dir "." []
fromLayer (Layer (a, sx)) = DTree $ a :/ (foldl1 insert . map singleton $ sx)
  where
    singleton :: (FilePath, DF) -> DirTree DF
    singleton (fp, df) = singleton' (splitPath fp, df)

    singleton' ([], _) = error "bug here"
    singleton' ([n], D) = Dir (dropTrailingPathSeparator n) []
    singleton' ([n], x) = File (dropTrailingPathSeparator n) x
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

test :: DTree DF -> Bool
test x = x == (fromLayer . toLayer $ x)

newtype Changes a = Changes [Diff (FilePath, DF)] deriving (Show, Generic, Eq)

newtype Layer a = Layer (FilePath, [(FilePath, DF)]) deriving (Show, Generic, Eq)

deriving instance Generic (Diff a)
instance Binary a => Binary (Diff a)
instance Binary a => Binary (Changes a)
instance Binary a => Binary (Layer a)

getPatch :: (Show a, Eq a) => Layer a -> Layer a -> Changes a
getPatch x y = Changes (filter isDifference $ getDiff xs ys)
  where
    (Layer (_, xs)) = sortLayer x
    (Layer (_, ys)) = sortLayer y
    isDifference Both{} = False
    isDifference _ = True

instance Eq a => Ord (Diff (FilePath, a)) where
    compare a b = compare (getFp a) (getFp b)
      where
        getFp (First (f, _)) = f
        getFp (Second (f, _)) = f
        getFp _ = error "can't use Both here"

diffShow :: Changes DF -> (DTree DF, DTree DF, DTree DF)
diffShow (Changes xs) = diffShow' (Set.empty, Set.empty, Set.empty) xs
    where
      diffShow' (l, r, lr) [] = ( fromLayer $ Layer ("(-)" , Set.toList $ Set.map fun l  )
                                , fromLayer $ Layer ("(+)" , Set.toList $ Set.map fun r  )
                                , fromLayer $ Layer ("(+-)", Set.toList $ Set.map fun lr )
                                )
      diffShow' (l, r, lr) (p@First{}:ys) = diffShow' (insertL (l, r, lr) p) ys
      diffShow' (l, r, lr) (p@Second{}:ys) = diffShow' (insertR (l, r, lr) p) ys
      diffShow' (l, r, lr) (_:ys) = diffShow' ( l, r, lr) ys
      fun (First a) = a
      fun (Second a) = a
      fun _ = error "can't be Both here"
      insertL (l, r, lr) p = if (Set.member p r)
                                then (l             , Set.delete p r, Set.insert p lr)
                                else (Set.insert p l,              r,              lr)
      insertR (l, r, lr) p = if (Set.member p l)
                                then (Set.delete p l,              r, Set.insert p lr)
                                else (l             , Set.insert p r,              lr)

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

sortLayer :: Layer a -> Layer a
sortLayer (Layer (x, xs)) = Layer (x, sortBy fun xs)
  where
  fun a b = compare (fst a) (fst b)




