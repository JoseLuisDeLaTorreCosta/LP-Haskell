eql :: [Int] -> [Int] -> Bool
eql l1 l2
    = l1 == l2


prod :: [Int] -> Int
prod list 
    = foldl (*) 1 list


prodOfEvens :: [Int] -> Int
prodOfEvens list 
    = prod (filter even list)


powersOf2 :: [Int]
powersOf2
    = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 
    = foldl (+) 0 (zipWith (*) l1 l2)