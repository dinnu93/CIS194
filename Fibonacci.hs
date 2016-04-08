-- Exercise-1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 = map fib [0..]

-- Exercise-2

fib2 = (let fibs a b = a : fibs b (a+b) in fibs 0 1)
fib2' = streamToList $ streamFromTwoSeeds (+) 0 1
-- Exercise-3

data Stream t = Cons t (Stream t)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons t s) = t : (streamToList s)

-- Exercise-4

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons t s) = Cons (f t) (streamMap f s) 

-- Produces an infinite stream with a function which generates next element
-- from a given element
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Useful for producing Fibonacci Stream
streamFromTwoSeeds :: (a -> a -> a) -> a -> a -> Stream a
streamFromTwoSeeds f x y = Cons x (streamFromTwoSeeds f y (f x y))

-- Exercise-5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Highest power of two which evenly divides n
highestPowerofTwo :: Integer -> Integer
highestPowerofTwo n = toInteger $ length $ fst $ break odd $ streamToList $ streamFromSeed (`div` 2) n

ruler :: Stream Integer
ruler = streamMap highestPowerofTwo $ streamFromSeed (+1) 1
