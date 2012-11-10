module LogFiles (FileInfo(..), printFileInfo, getLogFiles) where

import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM)
import System.IO (openFile, hClose, IOMode(..), hFileSize)
import Control.Exception (bracket, handle, IOException)
import Text.Printf (printf)

-- Really basic file metadata
data FileInfo = FileInfo {
    path :: FilePath
  , size :: Maybe Integer
} deriving (Show, Eq)

fileInfo :: FilePath -> IO FileInfo
fileInfo path =
  getSize path >>= \size ->
  return FileInfo {path=path, size=size}

printFileInfo :: FileInfo -> IO ()
printFileInfo (FileInfo path (Just size)) =
  putStrLn $ "logfile: "++path++" is " ++
    printf "%.2fMB" ((fromIntegral size) / 1048576 :: Double)
printFileInfo (FileInfo path Nothing) =
  putStrLn $ "logfile: "++path++" doesn't have a size."

getSize :: FilePath -> IO (Maybe Integer)
getSize path = handle errHandler $ 
  bracket (openFile path ReadMode) hClose $ \h -> do
    hFileSize h >>= \size -> return $ Just size
  where errHandler :: IOException -> IO (Maybe Integer)
        errHandler _ = return Nothing

ignoreVCS   = [ ".git", ".svn" ]
ignoreFiles = [ ".", ".." ]

-- Don't search up git directories.
canSearchDirectory :: FilePath -> Bool
canSearchDirectory dirName = dirName `notElem` ignoreVCS

-- We're looking for every log file over 1MB.
canGetFile :: FileInfo -> Bool
canGetFile (FileInfo path (Just size)) = (takeExtension path) `elem` [".log"] &&
                                         size > 1048576
canGetFile (FileInfo _ Nothing) = False

getLogFiles :: FilePath -> IO [FileInfo]
getLogFiles top =
  getDirectoryContents top >>= \paths ->
  return (filter (`notElem` ignoreFiles) paths) >>= \paths ->
  mapM recurse paths >>= \recursedPaths ->
  return (concat recursedPaths)
  where recurse :: FilePath -> IO [FileInfo]
        recurse path = 
          let fullPath = top </> path
           in doesDirectoryExist fullPath >>= \isDirectory ->
              if isDirectory
              then if canSearchDirectory path
                   then getLogFiles fullPath
                   else return []
              else fileInfo fullPath >>= \info ->
                   if canGetFile info
                   then return [info]
                   else return []
