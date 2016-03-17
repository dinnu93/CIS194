module LogAnalysis where
import Log

-- CIS 194 Chapter-2 Homework

-- Exercise-1

parseMessage :: String -> LogMessage
parseMessage s
  | null wordList = Unknown s
  | length firstWord /= 1 = Unknown s
  | firstWord == "E" = LogMessage (Error firstNumber) secondNumber errorMsg
  | firstWord == "I" = LogMessage Info firstNumber otherMsg
  | firstWord == "W" = LogMessage Warning firstNumber otherMsg
  | otherwise = Unknown s
  where wordList = words s
        firstWord = head wordList
        firstNumber =  read (wordList !! 1) :: Int
        secondNumber = read (wordList !! 2) :: Int
        errorMsg = unwords $ drop 3 wordList
        otherMsg = unwords $ drop 2 wordList

        
parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

-- Exercise-2
