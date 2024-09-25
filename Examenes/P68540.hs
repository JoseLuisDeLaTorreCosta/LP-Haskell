nsumsquare :: Integer -> Integer
nsumsquare n 
    = sum (map (\x -> x*x) [1..n])

nsquaresum :: Integer -> Integer
nsquaresum n 
    = x*x
    where 
        x = sum [1..n]

diffSqrs :: Integer -> Integer
diffSqrs n 
    = nsquaresum n - nsumsquare n


pythagoreanTriplets :: Int -> [(Int, Int, Int)]
pythagoreanTriplets n 
    = [(a, b, c) | a <- [1..n], b <- [1..n], a <= b, c <- [1..n], b <= c, a+b+c == n, a*a + b*b == c*c]


tartaglia :: [[Integer]]
tartaglia = iterate next [1]
    where 
        next xs = zipWith (+) ([0] ++ xs) (xs ++ [0])


sumDigits :: Integer -> Integer
sumDigits num 
    = foldl (\acc x -> acc + fromIntegral (read [x] :: Int)) 0 z
    where 
        z = show num


digitalRoot :: Integer -> Integer
digitalRoot
    = until (< 10) sumDigits