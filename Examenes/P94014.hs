fib2 :: Integer -> Integer -> Integer -> Integer
fib2 x y it
    |   it > 0 = fib2 y (x + y) (it-1)
    |   otherwise = x


fib :: Int -> Integer
fib x = fib2 0 1 (fromIntegral x)