{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module AccessMode where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 hiding (take, isEndOfLine)
import           Data.Attoparsec.Text (isEndOfLine)
import           Data.Bits
import           Data.Text            (Text)
import           Prelude              hiding (readFile, takeWhile)
import           System.Posix.Types
import           Text.Printf
import           Control.Monad (void)
import           Data.Binary
import           Data.ByteString (ByteString, cons)
import           GHC.Generics
import Data.Maybe
import Text.Regex.Posix.ByteString

example :: FilePath
example = "_fix_access_mode_"

type PathRegexp = ByteString

data Owner = NOwner ByteString
           | IOwner Integer
           deriving (Show, Eq, Ord, Generic)

data Group = NGroup ByteString
           | IGroup Integer
           deriving (Show, Eq, Ord, Generic)

instance Binary Group
instance Binary Owner

type Mode = (Owner, Group, Maybe CMode, Maybe CMode)

data AccessMode = AccessMode
  { umask     :: CMode
  , modes     :: [(PathRegexp, Mode)]
  , rawFile   :: ByteString
  } deriving (Eq, Ord, Generic)

instance Show AccessMode where
    show a = "Access mode: umask = " ++ cmodeToNumMode (umask a) ++ " modes = " ++ showModes 
      where
      showModes = 
        let m = modes a
            defMode = fromMaybe (umask a)
        in show $ map (\(p,(o, g, mfm, mgm)) -> show p ++ ":" ++ show o ++ ":" ++ show g ++ ":" ++ cmodeToNumMode (defMode mfm) ++ ":" ++ cmodeToNumMode (defMode mgm)) m

instance Binary CMode where
    put = put . fromEnum 
    get = toEnum <$> get

instance Binary AccessMode

comment :: Parser ()
comment = (emptySpace *> char '#' *> skipWhile (not . isEndOfLine)) *> pure ()

umaskP :: Parser CMode
umaskP = skipSpace *> "umask" *> delimeter *> numMode <* skipWhile isEndOfLine

goodUmask :: Text
goodUmask = "umask:177"

goodMode :: Text
goodMode = "/etc/*:chemist:users::755\n"

otherMode :: Text
otherMode = "/etc/*:chemist:users:644:\n"

emptySpace :: Parser ()
emptySpace = skipWhile (inClass " \n\t\r")

delimeter :: Parser ()
delimeter = skipSpace *> char ':' *> skipSpace *> pure ()

accessMode :: ByteString -> Parser AccessMode
accessMode r = do
    commentsOrSpaces
    x <- umaskP
    commentsOrSpaces
    modes' <-  mode `sepBy` commentsOrSpaces
    emptySpace
    endOfInput
    return $ AccessMode x modes' r 

commentsOrSpaces :: Parser ()
commentsOrSpaces = comments <|> emptySpace

comments :: Parser ()
comments = try (void $ (comment `sepBy` endOfLine) <* endOfLine <|> pure [()])

mode :: Parser (PathRegexp, Mode)
mode = do
    f <- char8 '/' <|> char8 '*'
    path <- takeWhile (\x -> not (inClass ":" x))
    delimeter
    ui <- userid
    delimeter
    gi <- groupid
    delimeter
    mm <- (Just <$> sfileMode) <|> pure Nothing
    delimeter
    mg <- (Just <$> try sfileMode) <|> pure Nothing
    return (cons f path, (ui, gi, mm, mg))

userid :: Parser Owner
userid = NOwner <$> takeWhile (\x -> not (inClass ":" x))

groupid :: Parser Group
groupid = NGroup <$> takeWhile (\x -> not (inClass ":" x))

sfileMode :: Parser CMode
sfileMode = numMode <|> symMode

numMode :: Parser CMode
numMode = numModeToCMode . digs <$> decimal
    where
    digs :: Integral x => x -> [x]
    digs 0 = [0]
    digs x = digs (x `div` 10) ++ [x `mod` 10]

symMode :: Parser CMode
symMode = textModeToCMode <$> count 9 allowed
  where
    allowed = satisfy (inClass "rwxstST-")

textModeToCMode :: String -> CMode
textModeToCMode xs =
    let z = complementBit 0 15
        withPos = zip [0..] $ reverse xs
        fun b (_, '-') = b
        fun b (i, 't') = complementBit (complementBit b i) 9
        fun b (_, 'T') = complementBit b 9
        fun b (i, 's')
          | i == 3 = complementBit (complementBit b i) 10
          | i == 6 = complementBit (complementBit b i) 11
          | otherwise = error "bad textmode"
        fun b (i, 'S')
          | i == 3 = complementBit b 10
          | i == 6 = complementBit b 11
          | otherwise = error "bad textmode"
        fun b (i, _) = complementBit b i
    in CMode $ foldl fun z withPos

numModeToCMode :: [Int] -> CMode
numModeToCMode xs =
    let z = complementBit 0 15
        withPos = zip [1 .. ] $ reverse xs
        fun b (_, 0) = b
        fun b (i, n) =
            let (nr, r) = if (n - 4) >= 0 then (n - 4, complementBit b (3 * i - 1)) else (n, b)
                (nw, w) = if (nr - 2) >= 0 then (nr - 2, complementBit r (3 * i - 2)) else (nr, r)
            in if nw == 1  then complementBit w (3 * i - 3) else w
    in CMode $ foldl fun z withPos

cmodeToTextMode :: CMode -> String
cmodeToTextMode (CMode y) =
   let asBool = zip [0 .. (11 :: Int)] $ reverse $ printf "%b" y
       sticky = (Just '1') == lookup 9 asBool
       geb    = (Just '1') == lookup 10 asBool
       ueb    = (Just '1') == lookup 11 asBool
       fun x (i, b)
         | sticky && i == 0 && b == '1' = 't' : x
         | sticky && i == 0 && b == '0' = 'T' : x
         | geb && i == 3 && b == '1' = 's' : x
         | geb && i == 3 && b == '0' = 'S' : x
         | ueb && i == 6 && b == '1' = 's' : x
         | ueb && i == 6 && b == '0' = 'S' : x
         | b == '1' && (i == 0 || i == 3 || i == 6) = 'x' : x
         | b == '1' && (i == 1 || i == 4 || i == 7) = 'w' : x
         | b == '1' && (i == 2 || i == 5 || i == 8) = 'r' : x
       fun x (i, _) = if i >= 9 then x else '-' : x
   in foldl fun "" asBool

cmodeToNumMode :: CMode -> String
cmodeToNumMode (CMode x) =
    let cm = [0, 0, 0, 0] :: [Int]
        asBool = zip [0 .. (11 :: Int)] $ reverse $ printf "%b" x
        posToNum i
          | i == 0 || i == 3 || i == 6 || i == 9 = 1
          | i == 1 || i == 4 || i == 7 || i == 10 = 2
          | i == 2 || i == 5 || i == 8 || i == 11 = 4
          | otherwise = 0
        fun (s:u:g:o:[]) (i, b)
          | b == '1' && i < 3 = [s, u, g, o + posToNum i]
          | b == '1' && i >= 2 && i < 6 = [s, u, g + posToNum i, o]
          | b == '1' && i >= 6 && i < 9 = [s, u + posToNum i, g, o]
          | b == '1' && i >= 9 && i < 12 = [s + posToNum i, u, g, o]
          | otherwise = [s, u, g, o]
        fun y _ = y
     in concatMap show $ foldl fun cm asBool

goodComment :: Text
goodComment = "  # asdfasdfasdf asdfasdf afsd\n\n"

makeAccessMode :: FilePath -> AccessMode -> IO ()
makeAccessMode = undefined

checkRegexp :: PathRegexp -> IO Bool
checkRegexp pr = do
   r <-  compile compBlank execBlank pr 
   case r of
       Left _ -> return False
       Right _ -> return True
