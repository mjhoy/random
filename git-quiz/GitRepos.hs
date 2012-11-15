module GitRepos (repoQuiz) where

import FoldDir (Iterator, Iterate(..), foldTree)
import FileInfo (Info(..))
import System.FilePath (takeFileName, takeDirectory)
import FileUtil (isDirectory)
import System.Cmd (rawSystem)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import ShellUtil (readShell)
import System.Random
import Control.Monad (foldM)
import Quiz (quiz)

gitReposIterator :: Iterator [FilePath]
gitReposIterator paths info
    | isDirectory info && takeFileName path == ".git"
      = Skip ((takeDirectory path):paths)
    | isDirectory info && (head (takeFileName path) == '.' ||
                           head (takeFileName path) == '_' )
      = Skip paths
    | otherwise 
      = Continue paths
  where path = infoPath info

getGitRepos :: FilePath -> IO [FilePath]
getGitRepos path = foldTree gitReposIterator [] path

-- Returns a list of commits for a git directory
listOfCommits :: FilePath -> IO [String]
listOfCommits path = getCurrentDirectory >>= \oldDir ->
  setCurrentDirectory path >>
  readShell "git log --format=\"%H\"" >>= \output ->
  setCurrentDirectory oldDir >>
  return (words output)

-- Returns a random commit for a git directory
randomCommit :: FilePath -> IO String
randomCommit path = listOfCommits path >>= \commits ->
  getStdRandom (randomR (0, (length commits) - 1)) >>= \i ->
  return (commits !! i)

-- Get a random repo for a base path, and return the
-- other repos as well
randomRepo :: FilePath -> IO (FilePath, [FilePath])
randomRepo path = getGitRepos path >>= \repos ->
  if (length repos) == 0
  then error "Darn. Couldn't find any repos."
  else getStdRandom (randomR (0, (length repos) - 1)) >>= \i ->
       let repo = repos !! i
       in return (repo, (filter (`notElem` [repo]) repos))


-- Returns a summary of a commit (for use in asking a question)
repoCommitSummary :: FilePath -> IO String
repoCommitSummary path = 
    repeat []
  where repeat tries =
          randomCommit path >>= \commit ->
          if commit `elem` tries
          then error "Darn. Couldn't find a commit."
          else
            getCurrentDirectory >>= \oldDir ->
            setCurrentDirectory path >>
            readShell ("git show " ++ commit ++ " | awk 'NR>10' | head -15") >>= \out ->
            setCurrentDirectory oldDir >>
            if length (words out) > 5
            then return out
            else repeat (commit:tries)

repoQuiz :: FilePath -> IO Bool
repoQuiz path = randomRepo path >>= \(repo, otherRepos) ->
  repoCommitSummary repo >>= \summary ->
  quiz summary repo otherRepos
