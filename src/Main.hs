module Main where

import System.Directory
import System.FilePath


path :: FilePath
path = "/Users/chemist/Develop/fix/tmp/"

realp :: FilePath
realp = path </> ".fix"


main :: IO ()
main = do
    createDirectoryIfMissing True realp
