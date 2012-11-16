module Quiz (quiz) where

import Shuffle (shuffle)
import Control.Monad (filterM, foldM, liftM)

-- Quiz!
type CorrectAnswer = String
type IncorrectAnswer = String
type Question = String

quiz :: Question -> CorrectAnswer -> [IncorrectAnswer] -> IO Bool
quiz question correct incorrects =
    putStrLn question >>
    ask
  where 
    ask =
      shuffleOptions >>= \options ->
      printOptions options >>
      getLine >>= \answer ->
      filterM (\(i,s) -> return $ i == (read answer)) options >>= \choice ->
      if (length choice) > 0
      then let answerString = snd $ head choice
               isCorrect = correct == answerString
           in if isCorrect
              then return True
              else return False
      else
           putStrLn "I don't understand. Try again." >>
           ask
    printOptions = mapM (\(n, s) -> putStrLn ("[" ++ (show n) ++ "]: " ++ s))
    options = correct : incorrects
    shuffleOptions =
        shuffle options >>= 
        liftM reverse . foldM acc []
      where
        acc list@((i,_):xs) s = return $ (i+1,s):list
        acc [] s = return $ (1, s):[]
