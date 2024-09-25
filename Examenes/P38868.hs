import Data.List

ascendent :: Int -> Int -> Ordering
ascendent a b 
    | a < b = LT 
    | otherwise = GT

sortAscent :: [Int] -> [Int] 
sortAscent
    = sortBy (ascendent)

descendent :: Int -> Int -> Ordering
descendent a b 
    | a > b = LT 
    | otherwise = GT

sortDescendect :: [Int] -> [Int] 
sortDescendect
    = sortBy descendent

minProd :: [Int] -> [Int] -> Int
minProd v1 v2 
    = sum [(l1 !! x)*(l2 !! x) | x <- [0..length l1 - 1]] 
    where 
        l1 = sortAscent v1 
        l2 = sortDescendect v2