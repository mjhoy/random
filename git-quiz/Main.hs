module Main (main) where

import System.Environment (getArgs)
import GitRepos (repoQuiz)

main = getArgs >>= \args ->
  processArgs args

processArgs (path:xs) = runQuiz path
processArgs [] = putStrLn $ "git-quiz 0.2\n\n" ++
  "usage: git-quiz path\n\n" ++
  "where `path` contains git repos as subdirectories.\n" ++
  "quizes your git knowledge."

runQuiz path = do
  answered <- repoQuiz path
  if (answered == True)
    then putStrLn "Hooray!"
    else putStrLn "Better luck next time!"
