fact1 :: Integer -> Integer
fact1 n = f !! fromIntegral n
    where 
        f = scanl (*) 1 [1..]


fact2 :: Integer -> Integer
fact2 1 = 1 
fact2 n = n*fact2 (n-1) 


fact3 :: Integer -> Integer
fact3 n = if n == 1 then 1 else n*fact3 (n-1)


fact4 :: Integer -> Integer
fact4 n = foldl (*) 1 [1..n]


fact5 :: Integer -> Integer
fact5 n = product [1..n]


fact6 :: Integer -> Integer
fact6 = fact1


fact7 :: Integer -> Integer
fact7 = fact1


fact8 :: Integer -> Integer
fact8 = fact1