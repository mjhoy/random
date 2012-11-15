module FileUtil (isDirectory, getUsefulContents) where

import System.Directory (getDirectoryContents, searchable)
import FileInfo (Info(..))

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)
