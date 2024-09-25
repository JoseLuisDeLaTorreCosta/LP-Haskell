divisors :: Int -> [Int]
divisors num 
    = [x | x <- [1..(div num 2)], mod num x == 0] ++ [num]


nbDivisors :: Int -> Int
nbDivisors = length . divisors


moltCompost :: Int -> Bool
moltCompost num 
    = maximum l == last l
        where 
            l = [nbDivisors x | x <- [1..num]]