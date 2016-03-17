module LogAnalysis where
import Log

-- CIS 194 Chapter-2 Homework

parseMessage :: String -> LogMessage
parseMessage s
  | null wordList = Unknown s
  | length firstWord /= 1 = Unknown s
  | firstWord == "E" = LogMessage (Error (read (wordList !! 1) :: Int)) (read (wordList !! 2) :: Int) (unwords (drop 3 wordList)) 
  | firstWord == "I" = LogMessage Info (read (wordList !! 1) :: Int) (unwords (drop 2 wordList))
  | firstWord == "W" = LogMessage Warning (read (wordList !! 1) :: Int) (unwords (drop 2 wordList))
  | otherwise = Unknown s
  where wordList = words s
        firstWord = head wordList


parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s
