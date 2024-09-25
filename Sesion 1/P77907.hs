absValue :: Int -> Int 
absValue x = if x <= 0 then -x else x


power :: Int -> Int -> Int
power _ 0 = 1
power x n =
    let 
        y = power x n_halved
        n_halved = div n 2
    in 
        if even n 
            then y * y
        else y * y * x


isPrime :: Int -> Bool
isPrime x = aux 2
    where
        aux d
            | x < 2 = False
            | x == d = True 
            | mod x d == 0 = False
            | otherwise = aux (d+1)




slowFib :: Int -> Int
slowFib x =
    let
        y = x - 1
        z = x - 2
    in
        if x < 2 then x
        else (slowFib y) + (slowFib z)



fib :: Int -> Int -> Int -> Int
fib x y it
    |   it > 0 = fib y (x + y) (it-1)
    |   otherwise = x


quickFib :: Int -> Int
quickFib x = fib 0 1 x
