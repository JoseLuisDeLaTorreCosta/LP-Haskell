myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs


myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc 
myFoldr f acc (l:ls)
    = f l (myFoldr f acc ls)


myIterate :: (a -> a) -> a -> [a]
myIterate f init = init:myIterate f (f init)


myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil while f acc
    | while acc = acc
    | otherwise = myUntil while f (f acc)


myMap :: (a -> b) -> [a] -> [b]
myMap f
    = myFoldl (\acc x -> acc ++ [f x]) []


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f 
    = myFoldl (\acc x -> if f x then acc ++ [x] else acc) []


myAll :: (a -> Bool) -> [a] -> Bool
myAll f l
    = and (map f l)


myAny :: (a -> Bool) -> [a] -> Bool
myAny f l
    = or (map f l)


myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y):myZip xs ys


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 
    = myMap (uncurry f) (myZip l1 l2)