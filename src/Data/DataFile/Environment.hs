{-# LANGUAGE DeriveGeneric #-}
module Data.DataFile.Environment where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString (ByteString, length)
import           Data.Yaml
import qualified Data.Yaml       as Y
import           GHC.Generics

import           Prelude         hiding (length)

data Env = Env
  { environment :: Value
  , rawFile     :: ByteString
  } deriving (Eq, Show, Generic)

instance Ord Env where
    compare x y = compare (rawFile x) (rawFile y)

instance Binary Env where
    put (Env _ x) = put (length x) >> putByteString x
    get = do
        l <- get
        bs <- getByteString l
        maybe (error "can't parse binary as Value") (return . flip Env bs) (Y.decode bs)




