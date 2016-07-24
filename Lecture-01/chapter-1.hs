-- Credit Card Validator
-- Exercise-1

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (quot n 10) ++ [(mod n 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)
    
-- Exercise-2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther zs = doubleEveryOther (init . init $ zs) ++ [(*2) . last .init $ zs, last zs]

-- Exercise-3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigits x)) + (sumDigits xs)

-- Exercise-4

validate :: Integer -> Bool
validate n = (mod (sumDigits (doubleEveryOther (toDigits n))) 10) == 0

-- Exercise-5

-- Towers of Hanoi Problem

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 first last temp = [(first,last)]
hanoi n first last temp = (hanoi (n-1) first temp last) ++ (hanoi 1 first last temp) ++ (hanoi (n-1) temp last first)
