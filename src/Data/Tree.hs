{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Tree where

import Data.Maybe (fromJust)
import Data.Monoid 
import Data.Binary
import GHC.Generics
 
type Name = String
type Route = [Name]

class Contexted a where
    index :: a -> Name
 
data Move b a = Next (b a)
              | Level (b a)
              deriving (Eq, Generic)

instance Binary a => Binary (Move Tree a)

instance (Show a, Show (b a)) => Show (Move b a) where
    show (Next x) = "\nNext " ++ show x
    show (Level x) = "\nLevel " ++ show x

type Moving b a = [Move b a]

type Zipper b a = (b a, Moving b a)

class Zippers b where
    toZipper :: b a -> Zipper b a
    attach :: Contexted a => a -> Zipper b a -> Zipper b a
    goNext :: Zipper b a -> Maybe (Zipper b a)
    goLevel :: Zipper b a -> Maybe (Zipper b a)
    goBack  :: Zipper b a -> Maybe (Zipper b a)
    goUp    :: Zipper b a -> Maybe (Zipper b a)
    top     :: Zipper b a -> Zipper b a
    route     :: Contexted a => Zipper b a -> Route
    value     :: Contexted a => Zipper b a -> Maybe a
    values :: Contexted a => Zipper b a -> [a]
    cursor  :: Contexted a => Zipper b a -> Maybe Name
    setCursor :: Contexted a => Route -> Zipper b a -> Maybe (Zipper b a)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Functor, Eq, Generic)

instance Binary a => Binary (Tree a)

instance Contexted a => Monoid (Tree a) where
    mempty = Empty
    mappend a Empty = a
    mappend Empty a = a
    mappend (Node v next link) x@(Node v1 next1 link1)
       | index v == index v1 = Node v (next <> next1) (link <> link1) 
       | otherwise = Node v (next <> x) link 


instance (Contexted a, Show a) => Show (Tree a) where
    show f = unlines $ drawLevel f
      where
        drawLevel Empty = []
        drawLevel (Node v next link) = (show (index v) <> " " <> show v <> " ") : (drawSubtree next link)

        drawSubtree next link = (shift "`- " " | " (drawLevel link)) <> drawLevel next
    
        shift first rest = zipWith (++) (first : repeat rest)

testTree :: Tree (String, Maybe String)
testTree = Node ("first", Just "first") (Node ("second", Just "second") Empty Empty) (Node ("third", Just "third") Empty Empty)

instance Contexted (String, Maybe String) where
    index (i,_) = i


goLastNext :: Zipper Tree a -> Zipper Tree a
goLastNext x = goLastNext' x True
  where
    goLastNext' t True =
        let maybeNext = goNext t
            maybeLevel = goLevel t
        in case (maybeLevel, maybeNext) of
                (Nothing, Nothing) -> t
                (Just n, _) -> goLastNext' n False
                (_, Just n) -> goLastNext' n False
    goLastNext' t False =
        let maybeNext = goNext t
        in case maybeNext of
                Nothing -> t
                Just n -> goLastNext' n False

instance Zippers Tree where
    toZipper t = (t, [])

    attach t (Empty, bs) = (Node t Empty Empty, bs)
    attach t (Node v next Empty, bs) = (Node v next (Node t Empty Empty), bs)
    attach t tr = 
      case isExist of
           Nothing ->
               let (Node v1 Empty link, bs) = goLastNext tr
               in fromJust $ setCursor way $ (Node v1 (Node t Empty Empty) link,bs)
           Just (Node _ next link, bs) -> fromJust $ setCursor way $ (Node t next link, bs)
           _ -> error "attach"
      where
        way = route tr
        isExist = setCursor (way <> [index t]) tr


    goNext (Empty, _) = Nothing
    goNext (Node _ Empty _, _) = Nothing
    goNext (Node x  next link, bs) = Just (next, Next (Node x   Empty link):bs)

    goLevel (Empty, _) = Nothing
    goLevel (Node _ _ Empty, _) = Nothing
    goLevel (Node x next link, bs) = Just (link, Level (Node x next Empty):bs)

    goBack (_, []) = Nothing
    goBack (t, Next  (Node x  Empty  link):bs) = Just (Node x  t    link, bs)
    goBack (t, Level (Node x  next  Empty):bs) = Just (Node x  next t   , bs)
    goBack _ = Nothing

    goUp (_, []) = Nothing
    goUp (t, Next  (Node x  Empty link ):bs) = goUp (Node x  t    link, bs)
    goUp (t, Level (Node x  next  Empty):bs) = Just (Node x  next t   , bs)
    goUp _ = Nothing

    value (Empty, _) = Nothing
    value (Node x _ _, _) = Just x

    top (t,[]) = (t,[])  
    top z = top (fromJust $ goBack z)

    route (z, m) = foldl fun [gi z] m
      where 
        fun xs (Next{}) = xs
        fun xs (Level x) = gi x : xs
        gi :: Contexted a => Tree a -> Name
        gi (Node x _ _) = index x
        gi _ = []

    values (Empty, []) = []
    values (z, m) = foldl fun [val z] m
      where
        fun xs (Next{}) = xs
        fun xs (Level x) = val x : xs
        val :: Contexted a => Tree a -> a
        val (Node x _ _) = x
        val _ = error "oid"

    setCursor [] z = Just (top z)
    setCursor ys z = walk ys (top z)
      where
      giz (Node x _ _  , _) = index x
      giz _ = error "setCursor: giz Empty Tree"
      walk [] t = Just t
      walk (x : []) t
        | x == giz t = Just t
        | otherwise = goNext t >>= walk (x : []) 
      walk (x : xs) t 
        | x == giz t = goLevel t >>= walk xs 
        | otherwise = goNext  t >>= walk (x : xs) 

    cursor ((Node v _ _), _) = Just (index v)
    cursor (Empty       , _) = Nothing

hasLevel :: Zipper Tree a -> Bool
hasLevel (Node _ _ Empty, _) = False
hasLevel _ = True

hasNext :: Zipper Tree a -> Bool
hasNext (Node _ Empty _, _) = False
hasNext _ = True

goClosest :: Contexted a => Route -> Zipper Tree a -> Zipper Tree a
goClosest [] z = top z
goClosest ys z = walk ys (top z)
  where
  giz (Node x _ _, _) = index x
  giz _ = error "goClosest: giz Empty Tree"
  walk [] z' = z'
  walk (x : []) z'
    | x == giz z' = z'
    | otherwise = maybe z' (walk (x : [])) (goNext z')
  walk (x : xs) z'
    | x == giz z' = maybe z' (walk xs) (goLevel z')
    | otherwise = maybe z' (walk (x : xs)) (goNext z')
                      

