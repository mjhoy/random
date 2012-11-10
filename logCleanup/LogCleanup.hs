-- logcleanup path
-- looks recursively through directories in path for
-- files that end in .log and are over 1MB, and clears them.

module Main (main) where

import LogFiles (getLogFiles, printFileInfo, FileInfo(..))
import Control.Monad (forM)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \args ->
  processArgs args

processArgs (x:xs) = processLogFiles x
processArgs [] = putStrLn $ "usage: logCleanup path\n\n" ++
  "looks recursively through directories in path for\n" ++
  "files that end in .log and that are over 1MB, and\n" ++
  "clears them."
                        
processLogFiles path = getLogFiles path >>= \logFiles ->
  (forM logFiles processLogFile) >> return ()

processLogFile fInfo = printFileInfo fInfo >>
  clearFile fInfo

clearFile (FileInfo path _) = writeFile path ""
