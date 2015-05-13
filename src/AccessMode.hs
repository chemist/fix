{-# LANGUAGE OverloadedStrings #-}
module AccessMode where

import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Text (Text)
import Data.Monoid ((<>))
import Prelude hiding (takeWhile)

type PathRegexp = String

data Owner = NOwner Text
           | IOwner Integer

data Group = NGroup Text
           | IGroup Integer

type FileMode = String

newtype AccessMode = AccessMode (Map PathRegexp (Owner, Group, FileMode))

main :: IO ()
main = putStrLn "hello"

comment :: Parser ()
comment = skipSpace *> char '#' *> skipWhile isEndOfLine *> pure ()

mode :: Parser (PathRegexp, (Owner, Group, FileMode))
mode = undefined

userid :: Parser Owner
userid = undefined

groupid :: Parser Group
groupid = undefined

fileMode :: Parser FileMode
fileMode = numMode <|> symMode

numMode :: Parser FileMode
numMode =  count 4 allowed <|> (("0" <>) `fmap` count 3 allowed)
    where
      allowed = satisfy (inClass "01234567")
    
      
goodNumMode :: Text
goodNumMode = "444"

goodTextMode :: Text 
goodTextMode = "rwxrwx---"

symMode :: Parser FileMode
symMode = count 9 allowed
  where
    allowed = satisfy (inClass "rwxstST-")

toTextMode :: String -> String
toTextMode "" = error "empty num mode"
toTextMode (s:xs) = sticky $ concatMap toMode xs
  where
    toMode :: Char -> String
    toMode '0' = "---" 
    toMode '1' = "--x" 
    toMode '2' = "-w-" 
    toMode '3' = "-wx" 
    toMode '4' = "r--" 
    toMode '5' = "r-x" 
    toMode '6' = "rw-" 
    toMode '7' = "rwx" 
    toMode _ = error "unknown mode"
    
    sticky :: String -> String
    sticky = snd . unzip . M.toList . fun s . M.fromList . zip [1..9 :: Int]
      where
        fun '0' m = m
        fun '1' m = if m ! 9 == '-'
                       then M.insert 9 'T' m
                       else M.insert 9 't' m
        fun '2' m = if m ! 6 == '-'
                       then M.insert 6 'S' m
                       else M.insert 6 's' m
        fun '4' m = if m ! 3 == '-'
                       then M.insert 3 'S' m
                       else M.insert 3 's' m
        fun '3' m = fun '1' . fun '2' $ m
        fun '5' m = fun '1' . fun '4' $ m
        fun '6' m = fun '2' . fun '4' $ m
        fun '7' m = fun '1' . fun '2' . fun '4' $ m
        fun _ _ = error "unknown sticky"

toNumMode :: String -> String
toNumMode "" = error "empty text mode"
toNumMode xs =
    let (owner, xs') = splitAt 3 xs
        (group, other) = splitAt 3 xs'
        allModes = [doble $ fun owner, fun group, fun other]
        sticky = head $ show $ foldl (\b a -> b + fst a) 0 allModes
        modes = concatMap (show . snd) allModes
    in sticky : modes
    where
      doble (x, y) = (2 * x, y)
      toMode '-' = (0, 0)
      toMode 'x' = (0, 1)
      toMode 'w' = (0, 2)
      toMode 'r' = (0, 4)
      toMode 't' = (1, 1)
      toMode 'T' = (1, 0)
      toMode 's' = (2, 1)
      toMode 'S' = (2, 0)
      toMode _ = error "bad mode"
      fun :: String -> (Int, Int)
      fun ys = foldl (\y x -> pluss y (toMode x)) (0,0) ys
      pluss :: (Int, Int) -> (Int, Int) -> (Int, Int)
      pluss (a,b) (c,d) = (a + c, b + d)

goodComment :: Text
goodComment = "  # asdfasdfasdf asdfasdf afsd\n\n"
