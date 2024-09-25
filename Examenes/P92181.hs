divisors :: Int -> [Int]
divisors num 
    = [x | x <- [1..(div num 2)], mod num x == 0]



analyze :: Int -> Either Int Bool
analyze num 
    | length l > 12 = Left (length l)
    where 
        l = divisors num