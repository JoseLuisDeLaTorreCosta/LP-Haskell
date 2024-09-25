sumMultiples :: Integer -> Integer -> Integer
sumMultiples x n = x * n' * (n' + 1) `div` 2
    where n' = div (n - 1) x

sumMultiples35 :: Integer -> Integer 
sumMultiples35 lim 
    = sumMultiples 3 lim + sumMultiples 5 lim - sumMultiples 15 lim


fib :: Integer -> Integer -> Int -> Integer
fib x y it
    |   it > 0 = fib y (x + y) (it-1)
    |   otherwise = x

fibonacci :: Int -> Integer
fibonacci = fib 0 1


fib2 :: Integer -> Integer -> Integer -> Integer
fib2 x y lim 
    | x >= lim = 0
    | even x = x + fib2 y (x + y) lim 
    | otherwise = fib2 y (x + y) lim 

sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis
    = fib2 0 1


largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = head $ filter (isPrime) $ filter (isFactor n) $ alReves n

alReves :: Integer -> [Integer]
alReves n = takeWhile (>0) $ iterate (sub) (n)

sub :: Integer -> Integer
sub x = x - 1

isFactor :: Integer -> Integer -> Bool
isFactor x y = x `mod` y == 0 

isPrimeRec :: Integer -> Integer -> Bool
isPrimeRec x div
    | div == 1 = True
    | x `mod` div == 0 = False
    | otherwise = isPrimeRec x (div - 1)

isPrime :: Integer -> Bool
isPrime x
    | x == 0 = False
    | x == 1 = True
    | otherwise = isPrimeRec x (floor (sqrt (fromIntegral x)))


isPalindromic2 :: String -> String -> Bool 
isPalindromic2 (x:xs) [] = False 
isPalindromic2 [] (x:xs) = False 
isPalindromic2 [x] [y] = x == y
isPalindromic2 (x:xs) (y:ys) 
    | x /= y = False
    | otherwise = isPalindromic2 xs ys

isPalindromic :: Integer -> Bool
isPalindromic num 
    = isPalindromic2 str (reverse str)
    where 
        str = show num