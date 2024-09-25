ones :: [Integer]
ones 
    = repeat 1

nats :: [Integer]
nats 
    = iterate (+1) 0

ints :: [Integer]
ints 
    = iterate (\x -> if x <= 0 then -x+1 else -x) 0


triangulars :: [Integer]
triangulars
    = scanl (+) 0 (iterate (+1) 1)


factorials :: [Integer]
factorials 
    = scanl (*) 1 (iterate (+1) 1)


fib :: Integer -> Integer -> Integer -> Integer
fib x y it
    |   it > 0 = fib y (x + y) (it-1)
    |   otherwise = x

quickFib :: Integer -> Integer
quickFib x = fib 0 1 x

fibs :: [Integer]
fibs 
    = [quickFib x | x <- nats]


isPrime :: Integer -> Bool
isPrime x = aux 2
    where
        aux d
            | x < 2 = False
            | x == d = True 
            | mod x d == 0 = False
            | otherwise = aux (d+1)

primes :: [Integer]
primes 
    = [x | x <- (iterate (+1) 2), (isPrime x)]


merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys)
    |   x < y = x : (merge xs (y:ys))
    |   x > y = y : (merge (x:xs) ys)
    |   otherwise = x : (merge xs ys)

hammings :: [Integer]
hammings
    = 1: map (*2) hammings `merge`  map (*3) hammings `merge` map (*5) hammings


nextLS :: [Char] -> [Char]
nextLS [] = []
nextLS x 
    = l ++ [num] ++ nextLS rest
    where 
        l = show (length (takeWhile (== head x) x))
        num = head x 
        rest = dropWhile (== head x) x


lookNsay :: [Integer]
lookNsay = iterate (\x -> read (nextLS (show x)) :: Integer) 1


nextT :: [Integer] -> [Integer]
nextT [] = []
nextT [x] = [] 
nextT (x:x2:xs) 
    = (x+x2):nextT (x2:xs)

tartaglia :: [[Integer]]
tartaglia = iterate (\x -> [1] ++ nextT x ++ [1]) [1]