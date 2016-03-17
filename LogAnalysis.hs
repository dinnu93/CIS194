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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ t _) msgTree@(Node lMsgTree nLogMessage@(LogMessage _ nT _) rMsgTree)
  | t < nT = Node (insert logMsg lMsgTree) nLogMessage rMsgTree
  | otherwise = Node lMsgTree nLogMessage (insert logMsg rMsgTree)

-- Exercise-3

build :: [LogMessage] -> MessageTree
build logMsgList = foldr insert Leaf logMsgList

-- Exercise-4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lMsgTree logMsg rMsgTree) = inOrder lMsgTree ++ [logMsg] ++ inOrder rMsgTree

-- Exercise-5

allowedLogMessage :: LogMessage -> Bool
allowedLogMessage (Unknown _) = False
allowedLogMessage (LogMessage (Error s) _ _)
  | s>50 = True
  | otherwise = False
allowedLogMessage (LogMessage _ _ _) = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsgList = map (\logMsg@(LogMessage _ _ msg) -> msg)  $ filter allowedLogMessage logMsgList
