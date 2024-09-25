import Data.List 

base :: Ord a => [a] -> Int -> a
base list pos = sort list !! (pos-1)


divide :: Ord a => [a] -> [[a]]
divide [] = []
divide list 
    = sort (take 5 list) : divide (drop 5 list)


takeMedians :: Ord a => [[a]] -> [a] 
takeMedians
    = foldl (\acc x -> acc ++ [x !! div (length x) 2]) []


select :: Ord a => [a] -> Int -> a
select list pos
    | length list < 5 = base list pos
    | rank == pos = takeMM
    | pos < rank = select (filter (< takeMM) list) pos 
    | otherwise = select (filter (> takeMM) list) (pos - rank)
    where 
        medians = sort (takeMedians (divide list))
        med = div (length medians) 2
        takeMM = medians !! med 
        rank = length (filter (<= takeMM) list)