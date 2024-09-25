
hanoi :: Integer -> String -> String -> String -> [(String, String)]
hanoi 0 or dest aux = []
hanoi num or dest aux 
    | num == 1 = [(or, dest)]
    | otherwise = hanoi (num-1) or aux dest ++ [(or, dest)] ++ hanoi (num-1) aux dest or

input2hanoi :: String -> [(String, String)]
input2hanoi str 
    = hanoi num or dest aux 
    where 
        towers = words str 
        num = read (head towers) :: Integer 
        or = towers !! 1
        dest = towers !! 2 
        aux = towers !! 3


main :: IO()
main = do 
    input <- getLine 
    let x = input2hanoi input
    mapM_ (\(y, z) -> putStrLn (y ++ " -> " ++ z)) x