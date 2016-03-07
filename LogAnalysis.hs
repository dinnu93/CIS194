module LogAnalysis where
import Log

-- CIS 194 Chapter-2 Homework

parseMessage :: String -> LogMessage
parseMessage s
  | ls !! 0 /= 1  = Unknown s
  where ls = [x | x <- [1..((length s)-1)], s !! x == ' ']
