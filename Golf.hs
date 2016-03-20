module Golf where
import qualified Data.List as L

-- Chapter-3 Exercises

-- Exercise-1

-- skips : Takes a list and gives list of lists of every element, every second element
-- to every nth element.

skips :: [a] -> [[a]]
skips ls = map (\m -> map (ls !!) $ filter (\n -> mod n (m+1) == m) iLs) iLs
  where iLs = [0..(length ls)-1]


-- Exercise-2

-- localMaxima is an element in the list which is greater than the element
-- after it and before it. The function localMaxima finds the list of all
-- localMaximas of an Integer list.

localMaxima :: [Integer] -> [Integer]
localMaxima ls = map (ls !!) $ filter (\i -> ((ls !! i) > (ls !! (i+1))) && ((ls !! i) > (ls !! (i-1)))) [1..(length ls)-2]

-- Exercise-3

-- histogram : which takes as input a list of Integer s between 0 and 9
-- (inclusive), and outputs a vertical histogram showing how many of each
-- number were in the input list

histogram :: [Int] -> String
histogram ls = foldl (++) [] $ map (++"\n") $ totalString
  where lenList = map (\e -> (head e,length e)) $ L.group . L.sort $ ls
        drawList = map (\e -> (e,0)) $ filter (not . (`elem` (L.nub ls))) $ [0..9]
        actList = map snd $ L.sort $ lenList ++ drawList
        maxFreq = maximum . (map snd) $ lenList
        endString = [replicate 10 '=' , (foldl (++) [] $ map show [0..9])]
        totalString = (L.transpose $  map (\i -> (replicate (maxFreq-i) ' ') ++ (replicate i '*')) actList) ++ endString

