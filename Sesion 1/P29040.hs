insert :: [Int] -> Int -> [Int]
insert list elem = init ++ [elem] ++ end 
    where 
        init = takeWhile (< elem) list 
        end = dropWhile (< elem) list 

isort :: [Int] -> [Int]
isort = foldl insert [] 


remove :: [Int] -> Int -> [Int]
remove (x:xs) elem 
    | x == elem = xs 
    | otherwise = x:remove xs elem

ssort :: [Int] -> [Int]
ssort [] = []
ssort list 
    = ssort (remove list max) ++ [max]
    where 
        max = maximum list


merge :: [Int] -> [Int] -> [Int]
merge l1 [] = l1
merge [] l2 = l2
merge (head1:tail1) (head2:tail2)
    | head1 <= head2 = [head1] ++ (merge tail1 (head2:tail2))
    | head1 >  head2 = [head2] ++ (merge (head1:tail1) tail2)

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x] 
msort l1 = merge (msort (take half l1)) (msort (drop half l1))
    where 
        half = div (length l1) 2


qsort :: [Int] -> [Int]
qsort [] = []
qsort [x] = [x]
qsort l1 = qsort (head l) ++ (l !! 1) ++ qsort (last l)
    where 
        l = foldl (\[lx, ly, lz] x -> if x < pivot then [x:lx, ly, lz] else if x == pivot then [lx, x:ly, lz] else [lx, ly, x:lz]) [[], [], []] l1
        pivot = div (sum l1) (length l1)


genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort [x] = [x]
genQsort l1 = genQsort (head l) ++ (l !! 1) ++ genQsort (last l)
    where 
        l = foldl (\[lx, ly, lz] x -> if x < pivot then [x:lx, ly, lz] else if x == pivot then [lx, x:ly, lz] else [lx, ly, x:lz]) [[], [], []] l1
        pivot = head l1
