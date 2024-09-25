countIf :: (Int -> Bool) -> [Int] -> Int
countIf f l = length (filter f l)


pam :: [Int] -> [Int -> Int] -> [[Int]]
pam list = map (`map` list)


pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 list flist
    = map (\x -> map (\f -> f x) flist) list


filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl ffil ffol ini list 
    = foldl ffol ini (filter ffil list)


insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert f list elem 
    = ini ++ [elem] ++ end 
    where 
        ini = takeWhile (`f` elem) list 
        end = dropWhile (`f` elem) list

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f
    = foldl (insert f) []