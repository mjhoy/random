module ShellUtil (readShell, readShellWithCode)  where

import qualified System.Process as P
import Control.Exception (evaluate, throwIO)
import System.IO (hClose, hGetContents)
import System.Exit (ExitCode(..))

-- Execute a shell command, capturing output and exit code.
readShellWithCode :: String -> IO (ExitCode, String)
readShellWithCode cmd = do
    (Nothing, Just hOut, Nothing, hProc) <- P.createProcess $
        (P.shell cmd) { P.std_out = P.CreatePipe }
    out <- hGetContents hOut
    _   <- evaluate (length out)
    hClose hOut
    ec  <- P.waitForProcess hProc
    return (ec, out)

readShell :: String -> IO String
readShell cmd = do
    (ec, out) <- readShellWithCode cmd
    case ec of
        ExitSuccess -> return out
        _ -> throwIO ec
