fizzBuzz :: [Either Int String]
fizzBuzz = map f [0..]
    where 
        f :: Int -> Either Int String
        f x 
            | mod x 15 == 0 = Right "FizzBuzz"
            | mod x 3 == 0 = Right "Fizz"
            | mod x 5 == 0 = Right "Buzz"
            | otherwise = Left x