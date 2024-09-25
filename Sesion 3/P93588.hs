myMap :: (a -> b) -> [a] -> [b]
myMap op list  
    = [(op x) | x <- list]


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter bool list
    = [x | x <- list, (bool x)]


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith op lista listb
    = [op (lista !! x) (listb !! x) | x <- (if ((length lista) < (length listb)) then [0..(length lista)-1] else [0..(length listb)-1])]


thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify lista listb 
    = [(x, y) | x <- lista, y <- listb, (mod x y == 0)]


factors :: Int -> [Int]
factors num 
    = [x | x <- [1..num], (mod num x == 0)]