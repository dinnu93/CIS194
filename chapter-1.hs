-- Credit Card Validator

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (quot n 10) ++ [(mod n 10)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)
    
lastSecond :: [a] -> a
lastSecond ls = head (drop ((length ls)-2) ls)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther zs = (doubleEveryOther (take ((length zs)-2) zs)) ++ [2*(lastSecond zs), (last zs)]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum (toDigits x)) + (sumDigits xs)

validate :: Integer -> Bool
validate n = (mod (sumDigits (doubleEveryOther (toDigits n))) 10) == 0

-- Towers of Hanoi Problem

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 first last temp = [(first,last)]
hanoi n first last temp = (hanoi (n-1) first temp last) ++ (hanoi 1 first last temp) ++ (hanoi (n-1) temp last first)
