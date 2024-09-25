myLength :: [Int] -> Int
myLength [] = 0
myLength (_:tail) = 1 + (myLength tail)


myMaximum2 :: [Int] -> Int -> Int -> Int -> Int 
myMaximum2 x it length max 
    |   it == length = max 
    |   (x !! it) > max = myMaximum2 x (it+1) length (x !! it)
    |   otherwise = myMaximum2 x (it+1) length max

myMaximum :: [Int] -> Int
myMaximum x 
    = myMaximum2 x 0 (myLength x) (x !! 0)


average :: [Int] -> Float
average x 
    = (fromIntegral (sum x)) / (fromIntegral (myLength x))


buildPalindrome :: [Int] -> [Int]
buildPalindrome x
    = reverse x ++ x


myRemove :: [Int] -> [Int] -> Int -> Int -> [Int]
myRemove x y it length
    |   (it >= length) = x
    |   (elem (x !! it) y) = myRemove ((take (it) x) ++ (drop (it+1) x)) y it (length-1)
    |   otherwise = myRemove x y (it+1) length

remove :: [Int] -> [Int] -> [Int]
remove x y
    = myRemove x y 0 (myLength x)


myFlatten :: [[Int]] -> Int -> Int -> [Int] -> [Int]
myFlatten x it length sol 
    |   (it == length) = sol
    |   otherwise = myFlatten x (it+1) length (sol ++ (x !! it))

flatten :: [[Int]] -> [Int]
flatten x
    = myFlatten x 0 (length x) []    


odds :: [Int] -> Int -> Int -> [Int] -> [Int]
odds x it length sol
    |   (it == length) = sol 
    |   (odd (x !! it)) = odds x (it+1) length (sol ++ [(x !! it)])
    |   otherwise = odds x (it+1) length sol

evens :: [Int] -> Int -> Int -> [Int] -> [Int]
evens x it length sol
    |   (it == length) = sol 
    |   (even (x !! it)) = evens x (it+1) length (sol ++ [(x !! it)])
    |   otherwise = evens x (it+1) length sol

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens x
    = (odds x 0 (myLength x) [], evens x 0 (myLength x) [])


isPrime :: Int -> Bool
isPrime x = aux 2
    where
        aux d
            | x < 2 = False
            | x == d = True 
            | mod x d == 0 = False
            | otherwise = aux (d+1)

myprimeDivisors :: Int -> Int -> [Int] -> [Int]
myprimeDivisors x divisor primeDiv 
    |   (x == 1) = primeDiv
    |   (not (isPrime divisor)) || (mod x divisor /= 0) = myprimeDivisors x (divisor+1) primeDiv
    |   (elem divisor primeDiv) = myprimeDivisors (div x divisor) 2 primeDiv
    |   otherwise = myprimeDivisors (div x divisor) 2 (primeDiv ++ [divisor])

primeDivisors :: Int -> [Int]
primeDivisors x
    = myprimeDivisors x 2 []