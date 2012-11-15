module Main (main) where

import System.Environment (getArgs)
import GitRepos (repoQuiz)

main = getArgs >>= \args ->
  processArgs args

processArgs (path:xs) = runQuiz path
processArgs [] = putStrLn $ "git-test 0.1\n\n" ++
  "usage: git-test path\n\n" ++
  "where `path` contains git repos as subdirectories.\n" ++
  "tests your git knowledge."

runQuiz path = do
  answered <- repoQuiz path
  if (answered == True)
    then putStrLn "Hooray!"
    else putStrLn "Better luck next time!"
