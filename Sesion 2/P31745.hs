flatten :: [[Int]] -> [Int]
flatten llist
    = foldl (++) [] llist


myLength :: String -> Int
myLength list
    = foldl (+) 0 (map (const 1) list)



myReverse :: [Int] -> [Int]
myReverse list 
    = foldl (\xs x -> x:xs) [] list


countIn :: [[Int]] -> Int -> [Int]
countIn llist elem 
    = map (\x -> length x) l 
    where 
        l = map (filter (== elem)) llist


firstWord :: String -> String
firstWord x
    = takeWhile (/= ' ') (dropWhile (== ' ') x)