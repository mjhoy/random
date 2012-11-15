module FileInfo (Info(..), getInfo) where

import System.Directory ( Permissions(..)
                        , getModificationTime
                        , getPermissions
                        )
import System.Time (ClockTime(..))
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Control.Monad (liftM)
import Control.Exception (handle, bracket, IOException)

data Info = Info {
  infoPath :: FilePath
, infoPerms :: Maybe Permissions
, infoSize :: Maybe Integer
, infoModTime :: Maybe ClockTime
} deriving (Eq, Ord, Show)

-- Takes an IO action; wraps the result in a Maybe and
-- returns Nothing if exception raised.
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle errHandler (Just `liftM` act)
  where errHandler :: IOException -> IO (Maybe a)
        errHandler _ = return Nothing

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)
