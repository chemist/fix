{-# LANGUAGE OverloadedStrings #-}
module AccessMode where

import Data.Attoparsec.Text hiding (take)
import Control.Applicative
import Data.Map (Map)
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
toTextMode (s:xs) = concatMap (toT s) $ zip [0 .. 3] xs
toTextMode _ = error "bad mode"

toT :: Char -> (Int, Char) -> String
toT s (i, '0') = "--" <> sticki s i "-"
toT s (i, '1') = "--" <> sticki s i "x"
toT s (i, '2') = "-w" <> sticki s i "-"
toT s (i, '3') = "-w" <> sticki s i "x"
toT s (i, '4') = "r-" <> sticki s i "-"
toT s (i, '5') = "r-" <> sticki s i "x"
toT s (i, '6') = "rw" <> sticki s i "-"
toT s (i, '7') = "rw" <> sticki s i "x"
toT _ _ = error "unknown mode"

sticki '0' _ x = x
sticki '1' 3 "-" = "T"
sticki '1' 3 _ = "t"
sticki '2' 2 "-" = "S"
sticki '2' 2 _ = "s"
sticki '3' 2 _ = "s"


goodComment :: Text
goodComment = "  # asdfasdfasdf asdfasdf afsd\n\n"
