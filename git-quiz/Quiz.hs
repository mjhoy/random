module Quiz (quiz) where

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
      printOptions >>
      getLine >>= \answer ->
      let answerNum = (read answer :: Int)
          choice = filter (\(i,s) -> i == answerNum) optionsWithIndexes
      in if (length choice) > 0
         then let answerString = snd $ head choice
                  isCorrect = correct == answerString
              in if isCorrect
                 then return True
                 else return False
         else
           putStrLn "I don't understand. Try again." >>
           ask
    printOptions =
      mapM (\(n, s) -> putStrLn ("[" ++ (show n) ++ "]: " ++ s)) optionsWithIndexes
    -- TODO: shuffle options.
    options = correct : incorrects
    optionsWithIndexes =
        foldl acc [] options
      where
        acc list@((i,_):xs) s = (i+1,s):list
        acc [] s = (1, s):[]
