import Data.List

degree :: Eq a => [(a, a)] -> a -> Int
degree [] _ = 0
degree ((x, y):xs) num 
    | x == num || y == num = 1 + degree xs num 
    | otherwise = degree xs num


degree' :: Eq a => [(a, a)] -> a -> Int
degree' list num 
    = foldl (\acc (x, y) -> if x == num || y == num then acc+1 else acc) 0 list


neighbors :: Ord a => [(a, a)] -> a -> [a]
neighbors list num 
    = sort $ foldr (\(x, y) acc  -> if x == num then y:acc else if y == num then x:acc else acc) [] list