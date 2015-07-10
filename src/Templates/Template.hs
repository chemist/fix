module Templates.Template where

import Text.EDE
import Data.ByteString.Char8
import Data.Binary

data Tpl = Tpl
  { template :: Template
  , rawFile  :: ByteString
  } deriving (Eq) 

instance Binary Tpl where
    put (Tpl _ x) = put x
    get = do
        b <- get
        case parse b of
             Success t -> return $ Tpl t b
             _ -> error "can't parse template"

instance Show Tpl where
    show (Tpl _ x) = " T " ++ unpack x


instance Ord Tpl where
    compare x y = compare (rawFile x) (rawFile y)

