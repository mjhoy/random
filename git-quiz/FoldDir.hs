-- from Real World Haskell

module FoldDir (foldTree, Iterator, Iterate(..)) where

import System.FilePath ((</>), takeExtension)
import FileInfo (Info(..), getInfo)
import Data.Char (toLower)
import FileUtil (isDirectory, getUsefulContents)
import Shuffle (shuffle)

data Iterate seed = Done { unwrap :: seed }
                  | Skip { unwrap :: seed }
                  | Continue { unwrap :: seed }
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
    endSeed <- fold initSeed path
    return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= shuffle >>= walk seed subpath
    walk seed subpath (name:names) = do
      let path' = subpath </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed'    -> walk seed' subpath names
        Continue seed'
          | isDirectory info -> do
            next <- fold seed' path'
            case next of
              done@(Done _) -> return done
              seed''        -> walk (unwrap seed'') subpath names
          | otherwise -> walk seed' subpath names
    walk seed subpath _ = return (Continue seed)
